//test return 17
#include <stdbool.h>
#include <stdlib.h>
extern void _c0_assert(bool param);

// How many dominos can be placed on an n x n chessboard with some squares removed?

/* flow code adapted from
http://aduni.org/courses/algorithms/courseware/handouts/Reciation_09.html */

/*#include <stdio.h>*/
/*#include <stdlib.h>*/
/*#include <stdbool.h>*/

struct queue {
  int head;
  int tail;
  int* data;
};

typedef struct queue queue_t;

struct flow_graph {
  int n;  // number of nodes
  int e;  // number of edges
  int** capacity; // capacity matrix
  int** flow;     // flow matrix
  int* color; // needed for breadth-first search
  int* pred;  // array to store augmenting path
  int _c0_checksum; // store answer
  queue_t* q;
};

typedef struct flow_graph graph_t;

int min (int x, int y) {
    return x<y ? x : y;  // returns minimum of x and y
}

void enqueue (queue_t* q, graph_t* g, int x) {
    q->data[q->tail] = x;
    q->tail += 1;
    g->color[x] = 1;
}

int dequeue (queue_t* q, graph_t* g) {
    int x = q->data[q->head];
    q->head += 1;
    g->color[x] = 2;
    return x;
}

bool bfs (graph_t *g, int start, int target) {
    int u;
    int v;
    for (u=0; u<g->n; u++) {
      g->color[u] = 0;
    }
    queue_t *q = g->q;
    enqueue(q, g, start);
    g->pred[start] = -1;
    while (q->head!=q->tail) {
      u = dequeue(q,g);
        // Search all adjacent white nodes v. If the capacity
        // from u to v in the residual network is positive,
        // enqueue v.
      for (v=0; v<g->n; v++) {
        if (g->color[v]==0 && g->capacity[u][v]-g->flow[u][v]>0) {
          enqueue(q, g, v);
          g->pred[v] = u;
        }
      }
    }
    // If the color of the target node is black now,
    // it means that we reached it.
    return g->color[target]==2;
}

int max_flow (graph_t* g, int source, int sink) {
    int i;
    int j;
    int u;
    // Initialize empty flow.
    int max_flow = 0;
    for (i=0; i<g->n; i++) {
      for (j=0; j<g->n; j++) {
          g->flow[i][j] = 0;
      }
    }
    // While there exists an augmenting path,
    // increment the flow along this path.
    int increment = 50;
    while (bfs(g,source,sink)) {
        // Determine the amount by which we can increment the flow.
      for (u=g->n-1; g->pred[u]>=0; u=g->pred[u]) {
          increment = min(increment,g->capacity[g->pred[u]][u]-g->flow[g->pred[u]][u]);
      }
        // Now increment the flow.
      for (u=g->n-1; g->pred[u]>=0; u=g->pred[u]) {
          g->flow[g->pred[u]][u] += increment;
          g->flow[u][g->pred[u]] -= increment;
      }
      max_flow += increment;
    }
    // No augmenting path anymore. We are done.
    return max_flow;
}

graph_t *_c0_init(int param) {
  graph_t *data = calloc(1, sizeof(graph_t));
  data->n = 40;
  data->e = (data->n * (data->n-1)) * 2;
  data->capacity = calloc(data->n * data->n + 2, sizeof(int*));
  data->flow = calloc(data->n * data->n + 2, sizeof(int*));
  data->color = calloc(data->n * data->n + 2, sizeof(int));
  data->pred = calloc(data->n * data->n + 2, sizeof(int));
  for (int i = 0 ; i < data->n*data->n + 2 ; i += 1) {
    data->capacity[i] = calloc(data->n * data->n + 2, sizeof(int));
    data->flow[i] = calloc(data->n * data->n + 2, sizeof(int));
  }
  data->q = calloc(1, sizeof(queue_t));
  data->q->data = calloc(1000000, sizeof(int));
  return data;
}

bool is_square(graph_t *data, int i, int j) {
  return i >= 0 && j >= 0 && i < data->n && j < data->n &&
    // remove a _c0_random-looking section of the board
    (((19*j + 17*i) % 30) % 7 != 3);
}

void add_edges(graph_t *data, int i, int j) {
  if (is_square(data, i-1, j)) {
    if (is_square(data, i, j)) {
      data->capacity[i*data->n + j][(i-1)*data->n + j] = 1;
    }
  }
  if (is_square(data, i+1, j)) {
    if (is_square(data, i, j)) {
      data->capacity[i*data->n + j][(i+1)*data->n + j] = 1;
    }
  }
  if (is_square(data, i, j-1)) {
    if (is_square(data, i, j)) {
      data->capacity[i*data->n + j][i*data->n + j-1] = 1;
    }
  }
  if (is_square(data, i, j+1)) {
    if (is_square(data, i, j)) {
      data->capacity[i*data->n + j][i*data->n + j+1] = 1;
    }
  }
  if (i+j % 2 == 0) {
    if (is_square(data, i, j)) {
      data->capacity[data->n*data->n][i*data->n + j] = 1;
      data->capacity[i*data->n + j][data->n*data->n] = 1;
    }
  }
  else {
    if (is_square(data, i, j)) {
      data->capacity[i*data->n + j][data->n*data->n+1] = 1;
      data->capacity[data->n*data->n+1][i*data->n + j] = 1;
    }
  }
}

void _c0_prepare(graph_t *data, int param) {
  data->n = 40;
  data->q->head = 0;
  data->q->tail = 0;
  for (int i = 0 ; i < data->n*data->n + 2 ; i += 1) {
    for (int j = 0 ; j < data->n*data->n + 2 ; j += 1) {
      data->capacity[i][j] = 0;
    }
    data->pred[i] = -1;
    data->color[i] = 0;
  }
  data->_c0_checksum = 0;
}

void _c0_run(graph_t *data, int param) {
  // Input is an n x n chessboard with some squares removed
  data->_c0_checksum = 0;
  for (int i = 0 ; i < data->n ; i += 1) {
    for (int j = 0 ; j < data->n ; j += 1) {
      add_edges(data, i, j);
    }
  }
  data->n = data->n * data->n + 2;
  data->_c0_checksum = max_flow(data, data->n-2, data->n-1);
}

int _c0_checksum(graph_t *data, int param) {
  return data->_c0_checksum;
}

int _c0_main() {
  graph_t *g = _c0_init(0);
  for (int _ = 0 ; _ < 10 ; _++) {
    _c0_prepare(g, _);
    _c0_run(g,_);
  }
  return _c0_checksum(g, 0);
}

