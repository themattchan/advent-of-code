#include <assert.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

typedef long long int R;

// doubly linked list ring buffer
typedef struct node
{
  int n;
  struct node * fwd;
  struct node * bwd;
} node;

node* singleton(int n)
{
   node * new = (node*)malloc(sizeof(node));
   new->n = n;
   new->fwd = new;
   new->bwd = new;
   return new;
}

// stick new node in front of a node, return new node
node * grow(int n, node * before)
{
  node * new = (node*)malloc(sizeof(node));

  // skip one if possible
  if (before->fwd !=before)
    before=before->fwd;

  new->n = n;

  new->fwd = before->fwd;
  new->bwd = before;

  //  assert(before->fwd->bwd == before);
  before->fwd->bwd = new;
  before->fwd = new;

  return new;
}

void free_node(node * n)
{
  if (!n) return;

  node* f = n->fwd;
  node* b = n->bwd;

  if (f) f->bwd = b;
  if (b) b->fwd = f;

  free(n);
}

void free_nodes(node *n)
{
  node * cur = n;
  while (cur && cur->fwd && cur->fwd != n) {
    node* p = cur;
    cur = cur->fwd;
    free(p);
  }
  free(cur);
}

// evict this node, go forward by 1
node * shrink(node * n, R* acc_score)
{
  (*acc_score) += n->n;
  //  printf("SHRINK: adding %d\n", n->n);
  node* ret = n->fwd;
  free_node(n);
  if (ret == n) return NULL;
  return ret;
}

node * shrink7(node *n, R *acc_score)
{
  node* cur = n;
  for (int i = 0; i < 7; i++) {
    cur = cur->bwd;
  }
  cur = shrink(cur, acc_score);
  return cur;
}

void print_buf(node* n)
{
  node* cur = n;
  do {
    printf("%d ", cur->n);
    cur = cur->fwd;
  } while (cur != n);
}

R doit(int players, int hi)
{
  R scores[players];// = {0};
  for (int i = 0; i < players; i++) {
    scores[i] = 0;
  }

  int cur_player = 0;
  int cur_marble = 1;
  node* circle = singleton(0);

  while (cur_marble <= hi) {
    //    printf("[%d]  (circle=%d)  ", cur_player,circle->n);
    //    print_buf(circle->fwd);

    if (cur_marble % 23 == 0) {
      circle = shrink7(circle, &scores[cur_player]);
      //printf("adding cur marble: %d\n",cur_marble);
      scores[cur_player] += cur_marble;
      cur_marble++;
    }
    else {
      circle = grow(cur_marble, circle);
      cur_marble++;
    }

    //printf("\nNEW circle=%d\n\n", circle->n);

    cur_player = (cur_player +1) % players;

  }

  R max = 0;
  for (int i = 0; i < players; i++) {
    //    printf("scores[%d]=%llu\n",i, scores[i]);
    max = scores[i] > max ? scores[i] : max;
  }
  //  printf("max=%llu\n\n",max);

  free_nodes(circle);

  return max;
}

int
main()
{
  int elfs, hi;

  FILE *fp;
  fp = fopen("input", "r");
  fscanf(fp, "%d players; last marble is worth %d points", &elfs, &hi);
  fclose(fp);

  {
    R _res;
#define TEST(x,y,z) {                                             \
      _res = doit(x,y);                                           \
      if (z == _res) printf("PASS (%d,%d) == %d\n", x,y,z);       \
      else printf("FAIL (%d,%d) != %d (GOT: %llu)\n", x,y,z,_res);  \
    }
    TEST(9,25,32);
    TEST(10,1618,8317);
    TEST(13,7999,146373);
    TEST(17,1104,2764);
    TEST(21,6111,54718);
    TEST(30,5807,37305);
#undef TEST
  }

  printf("\n");
  printf("INPUT: (%d, %d)\n", elfs, hi);
  printf("(part1) high score is: %llu\n", doit(elfs, hi));

  printf("\n");
  printf("INPUT: (%d, %d)\n", elfs, hi*100);
  printf("(part2) high score is: %llu\n", doit(elfs, hi * 100));


  return EXIT_SUCCESS;
}
