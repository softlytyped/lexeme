An OCaml interface and a C translation unit use the same naming algebra.

  $ cat > sample.mli <<'EOF'
  > val indices_at : int -> int list
  > val index_at : int -> int
  > val has_entries : int -> bool
  > EOF
  $ lexeme sample.mli
  sample.mli:1:5: note[plural-lift] indices_at: check whether this maps index_at over a collection (line 2)

  $ cat > included.h <<'EOF'
  > int entry_of(int x);
  > EOF
  $ cat > sample.c <<'EOF'
  > #include "included.h"
  > static int entries_of(int count) { return count; }
  > static int words_at(int count) { return count; }
  > static int word_at(int count) { return count; }
  > int test_and_set(int x /* lexeme: allow-letter hardware port */, int y) {
  >   int multiply = x;
  >   for (int i = 0; i < y; ++i) { multiply += i; }
  >   return multiply;
  > }
  > EOF
  $ lexeme sample.c
  sample.c:3:12: note[plural-lift] words_at: check whether this maps word_at over a collection (line 4)
  sample.c:5:70: error[single-letter] y: use a longer name or add a lexeme: allow-letter comment with a reason
  sample.c:6:7: error[variable-name] multiply: expected a noun phrase
  sample.c:7:12: error[single-letter] i: use a longer name or add a lexeme: allow-letter comment with a reason
  [1]

  $ echo 'let (' > broken.ml
  $ lexeme broken.ml >/dev/null 2>diagnostic
  [2]
  $ grep -q 'broken.ml' diagnostic && grep -qi 'syntax error' diagnostic

Compiler-inferred predicate types apply to local definitions, aliases and C typedefs.

  $ cat > predicates.ml <<'EOF'
  > type decision = bool
  > let is_ready (value : decision) = value
  > let has_value number = number + 1
  > let does_apply = has_value
  > let fold number = let is_even item = Some (item mod 2 = 0) in is_even number
  > EOF
  $ lexeme predicates.ml
  predicates.ml:3:5: error[predicate-result] has_value: expected a Boolean result, found int
  predicates.ml:4:5: error[predicate-result] does_apply: expected a Boolean result, found int
  predicates.ml:5:23: note[predicate-state] is_even: Are you sure Some false is a meaningfully different state from None? If so, you can ignore this note.
  [1]

  $ cat > predicates.h <<'EOF'
  > typedef _Bool verdict;
  > verdict is_ready(int number);
  > int has_value(int number);
  > EOF
  $ lexeme predicates.h
  predicates.h:3:5: error[predicate-result] has_value: expected a Boolean result, found int
  [1]

  $ mkdir project && cd project && git init -q
  $ echo 'let apply x = x' > owned.ml
  $ echo 'let apply z = z' > generated.ml
  $ echo generated.ml > .gitignore
  $ lexeme
  owned.ml:1:11: error[single-letter] x: use a longer name or add a lexeme: allow-letter comment with a reason
  [1]

C types, fields and enumeration constants are judged by their roles, and a function's
definition by its length. A Boolean field is a predicate, and main keeps its name.

  $ cat > shapes.c <<'EOF'
  > #include <stdbool.h>
  > typedef struct reading { int count; bool ready; bool is_open; } reading;
  > enum colour { RED, QUICKLY };
  > void report(int count);
  > int count_of(int total) {
  >   int sum = total;
  >   sum += 1;
  >   sum += 1;
  >   sum += 1;
  >   sum += 1;
  >   sum += 1;
  >   sum += 1;
  >   sum += 1;
  >   sum += 1;
  >   return sum;
  > }
  > int main(void) { return 0; }
  > EOF
  $ lexeme shapes.c
  shapes.c:2:42: error[predicate-name] ready: expected a predicate, such as is_empty or has_entries, for a Boolean
  shapes.c:3:20: error[constructor-name] QUICKLY: expected a noun phrase or an adjective phrase
  shapes.c:5:5: note[function-length] count_of: 12 source lines of code, more than 10
  [1]

Settings come from the nearest .lexeme in the working directory or one of its parents.

  $ mkdir -p settings/nested
  $ cat > settings/.lexeme <<'EOF'
  > ((vocabulary ((nouns (quux)))) (function_lines ((note 2) (warning 20) (error 30))))
  > EOF
  $ cat > settings/nested/sample.ml <<'EOF'
  > let quux = 0
  > let apply value =
  >   let product = value * 2 in
  >   product + 1
  > EOF
  $ cd settings/nested && lexeme sample.ml
  sample.ml:2:5: note[function-length] apply: 3 source lines of code, more than 2
