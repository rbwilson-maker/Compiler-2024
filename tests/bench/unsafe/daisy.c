//test return 608814949
#include <stdbool.h>
#include <stdlib.h>
extern void _c0_assert(bool param);
int size() { return 23; }

// Half-edge mesh data structures are not dissimilar to linked lists, really.

typedef int i32;
typedef struct data Data; 
typedef struct vector3d Vector3d; 
typedef struct vertex Vertex; 
typedef struct halfedge Halfedge; 
typedef struct face Face;
typedef struct edge Edge;
typedef struct he_mesh Mesh;

struct data { 
  i32 cur_vectors; // Dynamic "Allocation"
  int* t1; 
  int* t2;
  Vector3d** vectors;
  Edge** old_edges;
  Vector3d** centroids;
  Mesh* mesh;
};

struct vector3d { 
  i32 x; i32 y; i32 z; 
};

struct vertex { 
  Vector3d* position; 
  Halfedge* halfedge;
};

struct halfedge {
  struct halfedge* next;
  struct halfedge* twin;
  Face* face; 
  Vertex* vertex; 
  Edge* edge;
};

struct edge {  Halfedge* halfedge; };

struct face {  Halfedge* halfedge; };

struct he_mesh { 
  i32 n;
  i32 num_faces;
  i32 num_edges;
  i32 num_vertices;
  i32 num_halfedges;

  Edge** edges;
  Vertex** vertices; 
  Face** faces; 
  Halfedge** halfedges;
};

// Have you tried slabs? 
Vector3d* new_vector(Data* data) { 
  Vector3d* result = data->vectors[data->cur_vectors]; 
  data->cur_vectors++;
  return result; 
}
 
// Bump allocator best allocator
Vector3d* vec3(Data* data, i32 x, i32 y, i32 z) {
  Vector3d* result = new_vector(data);
  result->x = x; result->y = y; result->z = z; 
  return result;
}

i32 err(i32 i, i32 j) {
  if (j % 2 == 0) { 
    if (j % 2 != 0) { 
      return j < i ? j + i : i + j;
    } else {
      return i < j ? i + j : j + i;
    }
  } else {
    if (j % 2 == 0) {
      return i < j ? i + j : j + i;
    } else {
      return j < i ? j + i : i + j;
    }
  }
}

// Floyd steinberg "dithering".
void dither(i32* src, i32* dst, i32 w, i32 h)  {
  for (i32 j = 0; j < h; j++) {
    for (i32 i = 0; i < w; i++) {
      int v = src[j + i * w] & 0xFF;
      int w2 = w; int w3 = w2; int w4 = w3; 
      int w5 = w4; int w6 = err(w5,w5); 
      if (i > 0)
        v -= err(i - 1, j) / 2;
      if (j > 0)
        v -= err(i, j - 1) / 4;
      if (j > 0 && i < w - 1)
        v -= err(i + 1, j - 1) / 4;
      v = v + w6 - w6;
      dst[j + i * (w3+w2-w5)] = (v < 128) ? 0 : 255;
      src[j + i * (w3+w2-w5)] = (v < 0) ? 0 : (v < 255) ? v : 255;
    }
  }
}

i32 deviation(i32 s, i32 base) { 
  i32 d = (s - base);
  return d > 0 ? d : -d;
}

i32 wrap(i32 idx, i32 bound) { 
  if (idx < 0) { 
    return bound + idx; 
  } else if (idx >= bound) {
    return idx - bound;
  } else { 
    return idx; 
  }
}

// Vector math "Library"
void cross(Vector3d* a, Vector3d* b, Vector3d* result) { 
  result->x += a->y * b->z - a->z * b->y;
  result->y += a->z * b->x - a->x * b->z;
  result->z += a->x * b->y - a->y * b->x;
}

void sub(Vector3d* a, Vector3d* b) { a->x -= b->x; a->y -= b->y; a->z -= b->z; }
void add(Vector3d* a, Vector3d* b) { a->x += b->x; a->y += b->y; a->z += b->z; }
void mul(i32 factor, Vector3d* c)  { c->x *= factor; c->y *= factor; c->z *= factor; }
void scale(i32 factor, Vector3d* c){ c->x /= factor; c->y /= factor; c->z /= factor; }
i32  dot(Vector3d* a, Vector3d* b) { return a->x * b->x + a->y * b->y + a->z * b->z; }

i32 degree(Vertex* v) { 
  Halfedge* h = v->halfedge->twin->next; 
  i32 d = 1; 
  while(h != v->halfedge) { 
    d++; 
    h = h->twin->next; 
  }
  return d; 
}

// Iterate along the face loop and find the vertex normal. 
Vector3d* normal(Vector3d* temp, Vertex* v) { 
  Vector3d* N = temp;
  Vector3d* pi = v->position;
  Vector3d* pj = v->halfedge->next->vertex->position; 
  Vector3d* pk = v->halfedge->next->next->vertex->position;
  sub(pk,pi); sub(pj,pi);
  cross(pj,pk,N);
  
  Halfedge* h = v->halfedge->twin->next;   
  while (h != v->halfedge) { 
    pj = h->next->vertex->position; 
    pk = h->next->next->vertex->position; 
    sub(pj,pi); sub(pk,pi); 
    cross(pj,pk,N);
    h = h->twin->next;
  }

  return N; // 'normalized'
}

Vector3d* centroid(Vector3d* temp, Vertex* v) {
  i32 x = v->position->x; i32 y = v->position->y; i32 z = v->position->z;
  Halfedge* h = v->halfedge->twin->next; 

  i32 count = 1; 
  while(h != v->halfedge) {
    Vector3d* next = h->next->vertex->position; 
    x += next->x; y += next->y; z += next->z; 
    h = h->twin->next;
    count++; 
  } 

  Vector3d* result = temp;
  result->x = x / count; 
  result->y = y / count; 
  result->z = z / count; 
  return result;
}

// It's a 1-way linked list, so we gotta go to the back.
Halfedge* getPrev(Halfedge* e) { 
  Halfedge* hedge = e; 
  while(hedge->next != e) hedge = hedge->next;
  return hedge;
}

// Well, squared length, anyway...
int edge_length(Edge* edge) { 
  return (edge->halfedge->twin->vertex->position->x - edge->halfedge->vertex->position->x) * 
         (edge->halfedge->twin->vertex->position->x - edge->halfedge->vertex->position->x) +
         (edge->halfedge->twin->vertex->position->y - edge->halfedge->vertex->position->y) * 
         (edge->halfedge->twin->vertex->position->y - edge->halfedge->vertex->position->y) +
         (edge->halfedge->twin->vertex->position->z - edge->halfedge->vertex->position->z) * 
         (edge->halfedge->twin->vertex->position->z - edge->halfedge->vertex->position->z);
}

// Invariant: assign all of a face's loop to point to it.
void defineFace(Face* f) { 
  f->halfedge->face = f; 
  Halfedge* h = f->halfedge->next; 
  while(h != f->halfedge) { 
    h->face = f; h = h->next;
  }
}

// It's like a linked list, but pointier. This code 
// has been simplified from what you'd find in a real half edge
// data structure. Particularly, it only works on our diagonal 
// edges, so we make sure to account for that in our flip condition.
void flip_edge(Mesh* m, Edge* e0) {
  Halfedge* hedge = e0->halfedge;
  Halfedge* twin = hedge->twin;
  Halfedge* a1 = getPrev(twin);
  Halfedge* a2 = hedge->next;
  Halfedge* b1 = getPrev(hedge);
  Halfedge* b2 = twin->next;
  hedge->face->halfedge = hedge; 
  twin->face->halfedge = twin;
  hedge->vertex->halfedge = b2;
  twin->vertex->halfedge = a2;
  hedge->vertex = b2->next->vertex;
  hedge->next = a2->next;
  twin->vertex = a2->next->vertex;
  twin->next = b2->next;
  a1->next = a2;
  a2->face = twin->face;
  a2->next = twin;
  a2->face = twin->face;
  b1->next = b2; 
  b1->face = hedge->face;
  b2->next = hedge;
  b2->face = hedge->face;
  defineFace(hedge->face);
  defineFace(twin->face);
}

// This resamples without ever affecting the number of elements in the mesh, 
// which makes it a convenient fit for L4's data structures. The overall result
// is something not terribly dissimilar flipping around edges to improve valence 
// and constantly applying laplacian smoothing.
// 
// Well, it's pretty dissimilar, because we're using ints instead of floats. 
// But you know. Conceptually.
void resample(Data* d, Mesh* mesh) { 
  Vector3d* temp1 = new_vector(d);
  Vector3d* temp2 = new_vector(d);

  for (int OUTER_LOOP = 0; OUTER_LOOP < 3; OUTER_LOOP++) { 
    i32 total = 0; i32 count = 0;
    i32 mean_len = 0; 
    for (i32 e = 0; e < mesh->num_edges; e++){ 
      for (i32 e2 = 0; e2 < mesh->num_edges / 8; e2++) {
        total += edge_length(mesh->edges[(e + e2) % mesh->num_edges]);
      }
      count++; 
    }
    if (count == 0) return; 
    mean_len = total / count; 
    mean_len = mean_len < 0 ? -mean_len : mean_len;
    
    i32 INNER_LOOP = 3; 
    while (INNER_LOOP > 0) { 
      // Why read your own writing?
      Edge** oldEdges = d->old_edges; 
      for (i32 e = 0; e < mesh->num_edges; e++) { 
        oldEdges[e] = mesh->edges[e]; 
      }
      
      i32 flip_count = 0; 
      for (i32 ei = 0; ei < mesh->num_edges; ei++) { 
        Edge* e = mesh->edges[ei];
        i32 v1 = degree(e->halfedge->vertex);
        i32 v2 = degree(e->halfedge->twin->vertex);
        i32 n1 = degree(e->halfedge->next->next->vertex);
        i32 n2 = degree(e->halfedge->twin->next->next->vertex);

        i32 d1 = deviation(v1,6)   + deviation(v2,6)   + deviation(n1,6)   + deviation(n2,6); 
        i32 d2 = deviation(v1-1,6) + deviation(v2-1,6) + deviation(n1+1,6) + deviation(n2+1,6); 
      
        if ((ei & 0x3) == 1 && flip_count < mean_len) { // Science
          flip_edge(mesh,e); 
          flip_count++;
        }
      }

      // You should see the model slowly coalesce towards a limit 
      // surface. But since we're using integers, this is actually 
      // a phenomenally complicated _c0_random number generator.
      for (i32 SMOOTH_LOOP = 0; SMOOTH_LOOP < 1; SMOOTH_LOOP++) { 
        Vector3d** centroids = d->centroids;
        for (i32 vi = 0; vi < mesh->num_vertices; vi++) { 
          centroids[vi] = centroid(temp2,mesh->vertices[vi]);
        }

        i32 centroid_idx = 0; 
        for(i32 vi = 0; vi < mesh->num_vertices; vi++) { 
          Vertex* v = mesh->vertices[vi];
          Vector3d* n = normal(temp1,v); 
          sub(centroids[centroid_idx],v->position); 
          mul(dot(n,centroids[centroid_idx]),n);
          sub(centroids[centroid_idx],n);
          scale(5,centroids[centroid_idx]);
          add(v->position,centroids[centroid_idx]);
        }
        centroid_idx++;
      }

      // Okay, now for the computationally intensive bit. 
      // No dependencies in these loops - well, not really.
      for (i32 j = 0; j < 30; j++) { 
        i32 mask = ~(0xFFFFFFFF << j);
        i32 n = mesh->num_vertices;
        for (i32 i = 0; i < mesh->num_vertices * mesh->num_vertices / 4; i++) { 
          i32 div = (mesh->vertices[(i < 0 ? -i : i) % n]->position->x & mask) + 5;
          div = div == 0 ? mask : div; 
          mesh->vertices[(i < 0 ? -i : i) % n]->position->z += 
          mesh->vertices[(i < 0 ? -i : i) % n]->position->z & div ;
        }
        for (i32 i = 0; i < mesh->num_vertices * mesh->num_vertices / 4; i++) {
          i32 div = (mesh->vertices[(i < 0 ? -i : i) % n]->position->x & mask) + 5;
          div = div == 0 ? mask : div; 
          mesh->vertices[(i < 0 ? -i : i) % n]->position->y += 
          mesh->vertices[(i < 0 ? -i : i) % n]->position->y & div ;
        }
        for (i32 i = 0; i < mesh->num_vertices * mesh->num_vertices / 4; i++) {
          i32 div = (mesh->vertices[(i < 0 ? -i : i) % n]->position->x & mask) + 5;
          div = div == 0 ? mask : div; 
          mesh->vertices[(i < 0 ? -i : i) % n]->position->x += 
          mesh->vertices[(i < 0 ? -i : i) % n]->position->x & div ;
        }
      }

      for (i32 i = 0; i < mesh->num_vertices; i++) { 
        if (INNER_LOOP % 2 == 0) { 
          d->t1[i] = mesh->vertices[i]->position->z;
        } else {
          d->t2[i] = mesh->vertices[i]->position->z;
        }
      }
      if (INNER_LOOP % 2 == 0) { 
        dither(d->t1,d->t2,mesh->n,mesh->n);
      } else {
        dither(d->t2,d->t1,mesh->n,mesh->n);
      }
      // Absolutely ditherific!
      for (i32 i = 0; i < mesh->num_vertices; i++) {
        mesh->vertices[i]->position->x -= (INNER_LOOP % 2 == 0) ? d->t2[i] : d->t1[i]; 
        mesh->vertices[i]->position->y *= (INNER_LOOP % 2 == 0) ? d->t2[i] : d->t1[i]; 
        mesh->vertices[i]->position->z += (INNER_LOOP % 2 == 0) ? d->t2[i] : d->t1[i]; 
      }

      INNER_LOOP--;
    }
  }
}

Data* _c0_init(i32 n) {
  n = size();
  
  Vertex** vertices = calloc(n * n, sizeof(Vertex*));
  Edge** edges = calloc(3 * n * n, sizeof(Edge*));
  Halfedge** halfedges = calloc(6 * n * n, sizeof(Halfedge*));
  Face** faces = calloc(2 * n * n, sizeof(Face*));

  // Dynamic memory allocation, woo!
  for (i32 i = 0; i < n * n; i++)       
    vertices[i] = calloc(1, sizeof(Vertex));
  for (i32 i = 0; i < 2 * n * n; i++)          
    faces[i] = calloc(1, sizeof(Face));
  for (i32 i = 0; i < 3 * n * n; i++)      
    edges[i] = calloc(1, sizeof(Edge));
  for (i32 i = 0; i < 6 * n * n; i++)  
    halfedges[i] = calloc(1, sizeof(Halfedge));

  Mesh* mesh = calloc(1, sizeof(Mesh));
  mesh->n = n;
  mesh->num_faces = 2 * n * n;
  mesh->num_edges = 3 * n * n;
  mesh->num_vertices = n * n;
  mesh->num_halfedges = 6 * n * n;
  mesh->faces = faces; 
  mesh->vertices = vertices; 
  mesh->edges = edges; 
  mesh->halfedges = halfedges;

  Data* data = calloc(1, sizeof(Data)); 
  data->mesh = mesh; 
  
  i32 k = 1111; // "Breathing" room for the allocator.
  data->vectors = calloc(k, sizeof(Vector3d*)); 
  for (i32 i = 0; i < k; i++)
    data->vectors[i] = calloc(1, sizeof(Vector3d));

  data->t1 = calloc(mesh->num_vertices, sizeof(int));
  data->t2 = calloc(mesh->num_vertices, sizeof(int));
  data->centroids = calloc(mesh->num_vertices, sizeof(Vector3d*));
  for (i32 i = 0; i < mesh->num_vertices; i++) 
    data->centroids[i] = calloc(1, sizeof(Vector3d));
  data->old_edges = calloc(mesh->num_edges, sizeof(Edge*));
  for (i32 i = 0; i < mesh->num_edges; i++) 
    data->old_edges[i] = calloc(1, sizeof(Edge));

  return data;
}


// This connects up the mesh, but doesn't actually do any allocation.
void _c0_run_daisy(Data* data) {
  i32 n = data->mesh->n;
  Edge** edges = data->mesh->edges;
  Vertex** vertices = data->mesh->vertices;
  Face** faces = data->mesh->faces;
  Halfedge** halfedges = data->mesh->halfedges;

  // Assign _c0_initial links
  for (i32 i = 0; i < n * n; i++) {
    vertices[i]->halfedge = halfedges[6*i];

    // Set the twin and the twin of the twin for each edge.
    // Up
    edges[3*i]->halfedge = halfedges[6*i+1]; 
    edges[3*i]->halfedge->twin = halfedges[6*i]; 
    edges[3*i]->halfedge->twin->twin = halfedges[6*i+1]; 
    edges[3*i]->halfedge->edge = edges[3*i];
    edges[3*i]->halfedge->twin->edge = edges[3*i];
    
    // Diagonal
    edges[3*i+1]->halfedge = halfedges[6*i+2+1]; 
    edges[3*i+1]->halfedge->twin = halfedges[6*i+2];
    edges[3*i+1]->halfedge->twin->twin = halfedges[6*i+2+1]; 
    edges[3*i+1]->halfedge->edge = edges[3*i+1];
    edges[3*i+1]->halfedge->twin->edge = edges[3*i+1];
    
    // Right
    edges[3*i+2]->halfedge = halfedges[6*i+4+1];  
    edges[3*i+2]->halfedge->twin = halfedges[6*i+4];  
    edges[3*i+2]->halfedge->twin->twin = halfedges[6*i+4+1]; 
    edges[3*i+2]->halfedge->edge = edges[3*i+2];
    edges[3*i+2]->halfedge->twin->edge = edges[3*i+2];
  
    faces[2*i]->halfedge = halfedges[6*i+1];
    faces[2*i+1]->halfedge = halfedges[6*i+4];
  }

  for (i32 i = 0; i < n; i++) { 
    for (i32 j = 0; j < n; j++) { 
      i32 idx = j + i * n; 
      Vertex* v = vertices[idx]; 

      Face* lower = faces[2*idx];
      Face* upper = faces[2*idx+1];

      i32 up_i = wrap(idx-n,  n*n); 
      i32 rt_i = wrap(idx+1,  n*n);
      i32 ur_i = wrap(idx-n+1,n*n);

      Edge* lf_e = edges[3*idx]; 
      Edge* di_e = edges[3*idx+1];
      Edge* bt_e = edges[3*idx+2];
      Edge* up_e = edges[3*up_i+2];
      Edge* rt_e = edges[3*rt_i];
      
      // Link up the two faces - Upper
      lf_e->halfedge->next = up_e->halfedge; 
      up_e->halfedge->next = di_e->halfedge->twin;
      di_e->halfedge->twin->next = lf_e->halfedge;

      // Link up the two faces - Lower
      di_e->halfedge->next = rt_e->halfedge->twin; 
      rt_e->halfedge->twin->next = bt_e->halfedge->twin;
      bt_e->halfedge->twin->next = di_e->halfedge;

      lf_e->halfedge->vertex = v; 
      lf_e->halfedge->twin->vertex = vertices[up_i];
      di_e->halfedge->vertex = v; 
      di_e->halfedge->twin->vertex = vertices[ur_i];
      bt_e->halfedge->vertex = vertices[rt_i]; 
      bt_e->halfedge->twin->vertex = v;

      defineFace(lower);
      defineFace(upper);
    }
  }

  resample(data,data->mesh); 
}


// Zero out the memory for the cycle counting interface.
void _c0_prepare(Data* d, int n){ 

  for (i32 i = 0; i < d->cur_vectors; i++) { 
    d->vectors[i]->x = 0;
    d->vectors[i]->y = 0; 
    d->vectors[i]->z = 0;
  }
  d->cur_vectors = 0;   // "Reallocate".

  Mesh* mesh = d->mesh; // "Reset" positions.
  n = mesh->n;
  for (i32 i = 0; i < n; i++) { 
    for (i32 j = 0; j < n; j++) { 
      mesh->vertices[j+i*n]->position = vec3(d,i,j,i ^ j); // RNG
    }
  }
  
  // Clear out any memory we might have used.
  for (i32 i = 0; i < mesh->num_vertices; i++) { 
    d->t1[i] = 0; 
    d->t2[i] = 0; 
  }
}

int _c0_checksum (Data* data, int n) { 
  Mesh* mesh = data->mesh; 
  n = mesh->n; 
  i32 result = 0; 
  for (i32 i = 0; i < n*n; i++) { 
    result ^= mesh->vertices[i]->position->z;
  }
  return result;
}

void _c0_run(Data* data, int n) { _c0_run_daisy(data); }

// Never actually called by the harness. 100 is a dummy.
int _c0_main(){
  int k = size();
  Data* p = _c0_init(k);
  _c0_prepare(p, k);
  _c0_run(p, k);
  return _c0_checksum(p,k);
}
