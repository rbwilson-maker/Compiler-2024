//test return 334620
#include <stdbool.h>
#include <stdlib.h>
extern void _c0_assert(bool param);
int size() { return 5000000; }
/*
           An implementation of top-down splaying with sizes
             D. Sleator <sleator@cs.cmu.edu>, January 1994.
        
  This extends top-down-splay.c to _c0_maintain a size field in each node.
  This is the number of nodes in the subtree rooted there.  This makes
  it possible to efficiently compute the rank of a key.  (The rank is
  the number of nodes to the left of the given key.)  It it also
  possible to quickly find the node of a given rank.  Both of these
  operations are illustrated in the code below. The re_c0_mainder of this
  introduction is taken from top-down-splay.c.

  "Splay trees", or "self-adjusting search trees" are a simple and
  efficient data structure for storing an ordered set.  The data
  structure consists of a binary tree, with no additional fields.  It
  allows searching, insertion, deletion, deletemin, deletemax,
  splitting, joining, and many other operations, all with amortized
  logarithmic performance.  Since the trees adapt to the sequence of
  requests, their performance on real access patterns is typically even
  better.  Splay trees are described in a number of texts and papers
  [1,2,3,4].

  The code here is adapted from simple top-down splay, at the bottom of
  page 669 of [2].  It can be obtained via anonymous ftp from
  spade.pc.cs.cmu.edu in directory /usr/sleator/public.

  The chief modification here is that the splay operation works even if the
  item being splayed is not in the tree, and even if the tree root of the
  tree is NULL.  So the line:

                              t = splay(i, t);*

  causes it to search for item with key i in the tree rooted at t.  If it's
  there, it is splayed to the root.  If it isn't there, then the node put
  at the root is the last one before NULL that would have been reached in a
  normal binary search for i.  (It's a neighbor of i in the tree.)  This
  allows many other operations to be easily implemented, as shown below.

  [1] "Data Structures and Their Algorithms", Lewis and Denenberg,
       Harper Collins, 1991, pp 243-251.
  [2] "Self-adjusting Binary Search Trees" Sleator and Tarjan,
       JACM Volume 32, No 3, July 1985, pp 652-686.
  [3] "Data Structure and Algorithm Analysis", Mark Weiss,
       Benjamin Cummins, 1992, pp 119-130.
  [4] "Data Structures, Algorithms, and Performance", Derick Wood,
       Addison-Wesley, 1993, pp 367-375

  The following code was written by Daniel Sleator, and is released
  in the public do_c0_main. 

  * It was modified to the language L4 by the 15-411 course staff,
  attempting to preserve the spirit of Danny's original implementation
  as much as is feasible. L4's restrictions regarding large types on the
  stack has reduced some of the elegance, but not the amortized logarithmic
  behavior! 
*/

typedef struct tree_node Tree;
struct tree_node {
    Tree* left; 
    Tree* right;
    int key;
    int size;   /* _c0_maintained to be the number of nodes rooted here */
};

typedef struct data Data;
struct data { 
  Tree* temp; // No large types on the stack.
  Tree* root;
  Tree** nodes;
  int cur_nodes;
  int t; 
};

/* Drop in malloc replacement, for cool cats only. */
Tree* new_node(Data* data) { 
  Tree* result = data->nodes[data->cur_nodes]; 
  data->cur_nodes++; 
  return result;
}


/* This is the comparison.                                       */
/* Returns <0 if i<j, =0 if i=j, and >0 if i>j                   */
int compare(int i,int j) { return i-j; }

 
/* This macro returns the size of a node.  Unlike "x->size",     */
/* it works even if x=NULL.  The test could be avoided by using  */
/* a special version of NULL which was a real node with size 0.  */
int node_size(Tree* x) { return (((x)==NULL) ? 0 : ((x)->size)); }

 
/* Splay using the key i (which may or may not be in the tree.) */
/* The starting root is t, and the tree used is defined by rat  */
/* size fields are _c0_maintained */
Tree* splay (Tree* temp, int i, Tree* t) {
    Tree* N_left; Tree* N_right;
    Tree* l; Tree* r; Tree* y;
    int comp; int root_size; int l_size; int r_size;
    
    if (t == NULL) return t;
    N_left = NULL; N_right = NULL; 
    l = temp; r = temp;

    root_size = node_size(t);
    l_size = 0; r_size = 0;

    bool yes = true;
    while(yes) {
        Tree* k = NULL;
        comp = compare(i, t->key);
        if (comp < 0) {
            if (t->left == NULL) { yes = false; }
            else if (compare(i, t->left->key) < 0) {
              k = t->left; 
              y = k;                           /* rotate right */
              t->left = y->right;
              k = y->right;
              y->right = t;
              k = t; 
              t->size = node_size(t->left) + node_size(t->right) + 1;
              t = y;
              k = t->left;
              if (t->left == NULL) {
                yes = false;
              }
            }
            if (yes) {
              r->left = t;                               /* link right */
              r = t;
              t = t->left;
              r_size += 1+node_size(r->right);
            }
        } else if (comp > 0) {
            if (t->right == NULL) { yes = false; }
            else if (compare(i, t->right->key) > 0) {
              Tree* d; 
              y = t->right;                          /* rotate left */
              d = y; 
              t->right = y->left;
              d = t->right; d = y->left;
              y->left = t;
              d = y->left;
              t->size = node_size(t->left) + node_size(t->right) + 1;
              t = y; d = t; d = y; d = t->right;
              if (t->right == NULL) {
                yes = false;
              }
            }
            if (!yes) k = k == k ? k : k;
            if (yes) {
              l->right = t;                              /* link left */
              l = t;
              t = t->right;
              l_size += 1+node_size(l->left);
            }
        } else {
            yes = false;
        }
    }
    l_size += node_size(t->left);  /* Now l_size and r_size are the sizes of */
    r_size += node_size(t->right); /* the left and right trees we just built.*/
    t->size = l_size + r_size + 1;

    l->right = NULL; r->left = NULL;

    /* The following two loops correct the size fields of the right path  */
    /* from the left child of the root and the right path from the left   */
    /* child of the root.                                                 */
    for (y = N_right; y != NULL; y = y->right) {
        y->size = l_size;
        l_size -= 1+node_size(y->left);
    }
    for (y = N_left; y != NULL; y = y->left) {
        y->size = r_size;
        r_size -= 1+node_size(y->right);
    }
 
    l->right = t->left;                                /* assemble */
    r->left = t->right;
    t->left = N_right;
    t->right = N_left;

    return t;
}

/* Insert key i into the tree t, if it is not already there. */
/* Return a pointer to the resulting tree.                   */
Tree* insert(Data* data,int i, Tree * t) {
    if (t != NULL) {
	    t = splay(data->temp,i,t);
  	  if (compare(i, t->key)==0) {
        return t;  /* it's already there */
  	  }
    }
    Tree* new = new_node(data);
    if (new == NULL) { return NULL; }
    if (t == NULL) {
	    new->left = NULL; new->right = NULL;
    } else if (compare(i, t->key) < 0) {
	    new->left = t->left;
	    new->right = t;
	    t->left = NULL;
	    t->size = 1+node_size(t->right);
    } else {
	    new->right = t->right;
	    new->left = t;
	    t->right = NULL;
	    t->size = 1+node_size(t->left);
    }
    new->key = i;
    new->size = 1 + node_size(new->left) + node_size(new->right);
    return new;
}

/* Deletes i from the tree if it's there.               */
/* Return a pointer to the resulting tree.              */
Tree* delete(Data* data, int i, Tree *t) {
    Tree * x;
    int tsize;

    if (t==NULL) return NULL;
    tsize = t->size;
    t = splay(data->temp,i,t);
    if (compare(i, t->key) == 0) {               /* found it */
      if (t->left == NULL) {
          x = t->right;
      } else {
          x = splay(data->temp, i, t->left);
          x->right = t->right;
      }
      // free(t); L4 malloc best malloc
      if (x != NULL) {
          x->size = tsize-1;
      }
	    return x;
    } else {
	    return t;                         /* It wasn't there */
    }
}

/* Returns a pointer to the node in the tree with the given rank.  */
/* Returns NULL if there is no such node.                          */
/* Does not change the tree.  To guarantee logarithmic behavior,   */
/* the node found here should be splayed to the root.              */
Tree* find_rank(int r, Tree *t) {
    int lsize;
    if ((r < 0) || (r >= node_size(t))) return NULL;
    while (true) {
      lsize = node_size(t->left);
      if (r < lsize) {
        t = t->left;
      } else if (r > lsize) {
        r = r - lsize -1;
        t = t->right;
      } else {
        return t;
      }
    }
    return NULL;
}

/* A sample use of these functions.  Start with the empty tree,         */
/* insert some stuff into it, and then delete it.                       */
void splay_me_a_treap(Data* data) {
    Tree* root; Tree* t;
    int i;
    root = NULL;              /* the empty tree */
    for (i = 0; i < size() - 10; i++) {
	    root = insert(data,(541*i) % (1048575), root);
    }

    for (i = -1; i <= size() - 43; i++) {
	    t = find_rank(i, root);
      if (t != NULL) data->t += t->key;
    }

    for (i = 0; i < size() / 114; i += 114) {
	    root = delete(data,(541*i) & (1048575), root);
    }

    for (i=0; i < size() - 1; i++) {
      if (i % 2 == 0) {
	      root = splay(data->temp, i, root);
      } else {
        root = splay(data->temp, size() - 1 - i, root);
      }
    }

    data->root = root;
}

Data* _c0_init(int n) { 
  n = size();
  Data* data = calloc(1, sizeof(Data)); 
  data->temp = calloc(1, sizeof(Tree));
  data->root = NULL; 
  data->nodes = calloc(n, sizeof(Tree*)); 
  for (int i = 0; i < n; i++){ 
    data->nodes[i] = calloc(1, sizeof(Tree));
  }
  data->cur_nodes = 0;
  data->t = 0;
  return data;
}

void reset_node(Tree* tree) { 
  if (tree != NULL) {
    tree->key = 0; 
    tree->size = 0; 
    tree->left = NULL; 
    tree->right = NULL;
  }
}

void _c0_prepare(Data* data, int n) { 
  n = size();
  reset_node(data->temp);
  reset_node(data->root);
  for (int i = 0; i < n; i++) { 
    reset_node(data->nodes[i]);
  }
  data->cur_nodes = 0;
}

void _c0_run(Data* data, int n) { 
  splay_me_a_treap(data);
}

int _c0_checksum(Data* data, int n) { 
  return (data->root->key + data->root->size) ^ data->t;
}

int _c0_main() {
  int k = size();
  Data* data = _c0_init(k);
  _c0_prepare(data, k);
  _c0_run(data, k); 
  return _c0_checksum(data,k);
}
