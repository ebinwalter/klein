## Klein
Klein is a toy compiler for a C-like programming language targeting MIPS32 assembly.  The skeleton of the design is based on Bach, a much more minimal C-like language developed in CS 536 at the University of Wisconsin-Madison.  My improvements over what we implemented before the semester ended include:

- codegen for structs
- var decls anywhere within a code block
- pointers
- arrays
- register allocation
- string input
- character literals
- better syntax, in my opinion

### Usage
To compile a `.kl` file, use `cargo run -- <file>`.  A file with an identical name but an extension of '.s' will appear alongside it if compilation was successful.

When a program is compiled using a debug build, `kleinc` will "unparse" the program into `stdout` with annotations about offsets of local variables and field accesses.

### Examples
#### `tests/linked_list.kl`
```c
struct LinkedList {
  struct LinkedList *next;
  void *item;
}

int count_1;
int count_2;
int sum_2;
struct LinkedList[50] ll_heap;

fun main() {
  int i = 0;
  while i < 49 {
    ll_heap[i].next = &ll_heap[i+1];
    ll_heap[i].item = i;
    i = i + 1;
    out <- i;
    out <- "\n";
  }
  ll_heap[9].next = null;
  int count = 0;
  struct LinkedList *current = &ll_heap[0];
  while current != null {
    // pointers auto-deref with .
    current = current.next;
    count = count + 1;
  }
  count_1 = count;
  ll_heap[29].next = null;
  ll_heap[9].next = &ll_heap[20];
  int sum = 0;
  count = 0;
  current = &ll_heap[0];
  int item;
  while current != null {
    item = current.item;
    sum = sum + item;
    current = current.next;
    count = count + 1;
  }
  sum_2 = sum;
  count_2 = count;
}
```

#### `tests/str_cpy.kl`
```c
fun strcpy(char *dest, char *src) {
  int ix = 0;
  while src[ix] != '\0' {
    dest[ix] = src[ix];
    ix = ix + 1;
  }
  dest[ix] = '\0';
}

// Requires char literals to compile
fun strncpy(char *dest, char *src, int n) {
  int ix = 0;
  while src[ix] != '\0' {
    if ix < n - 1 {
      dest[ix] = src[ix];
    }
    ix = ix + 1;
  }
  dest[ix] = '\0';
}

fun strlen(char *s) -> int {
  int i = 0;
  while s[i] != '\0' {
    i = i + 1;
  }
  return i;
}

// assumes head has enough space to accommodate tail
fun strcat(char *head, char *tail) {
  int len = strlen(head);
  int i = 0;
  while tail[i] != '\0' {
    head[len+i] = tail[i];
    i = i + 1;
  }
  head[len+i] = '\0';
}

char[20] snup_buf;
char[20] usual_buf;

fun main() {
  char *snupis = &snup_buf[0];
  char *usual = &usual_buf[0];
  out <- strlen("Hfeflflfo");
  out <- "\n";
  strcpy(snupis, "hello ");
  strcpy(usual, "user");
  strcat(snupis, usual);
  strcat(snupis, "\n");
  out <- snupis;
}
```
