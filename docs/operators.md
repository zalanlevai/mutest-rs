# Mutation Operators

mutest-rs relies on a set of mutation operators to produce mutations for certain patterns of code. Each mutation operator is able to match on code fragments, based on syntax and compiler information (e.g. types, lifetimes, scope), and produce corresponding mutated code fragments as output.

Following is the list of mutation operators currently implemented in mutest-rs.

> [!NOTE]
> Replacements are illustrative and are meant to show how code behaviour effectively changes with each mutation.

## `arg_default_shadow`

Replace the provided arguments of functions with `Default::default()` to check if each parameter is tested with meaningful values.

This is done by rebinding parameters at the beginning of the function.

Replaces
```rs
fn foo(hash: u64) {
```
with
```rs
fn foo(hash: u64) {
    let hash: u64 = Default::default();
```

## `bit_op_or_and_swap`

Swap bitwise OR for bitwise AND and vice versa.

Replaces
```rs
byte & (0x1 << 2)
```
with
```rs
byte | (0x1 << 2)
```

## `bit_op_or_xor_swap`

Swap bitwise OR for bitwise XOR and vice versa.

Replaces
```rs
bytes[i] |= 0x1 << 3
```
with
```rs
bytes[i] ^= 0x1 << 3
```

## `bit_op_shift_dir_swap`

Swap the direction of bitwise shift operators.

Replaces
```rs
byte & (0x1 << i)
```
with
```rs
byte & (0x1 >> i)
```

## `bit_op_xor_and_swap`

Swap bitwise XOR for bitwise AND and vice versa.

Replaces
```rs
byte & (0x1 << 2)
```
with
```rs
byte ^ (0x1 << 2)
```

## `bool_expr_negate`

Negate boolean expressions.

Replaces
```rs
if !handle.is_active() {
    drop(handle);
```
with
```rs
if handle.is_active() {
    drop(handle);
```

## `call_delete`

Delete function calls and replace them with `Default::default()` to test whether inner calls are meaningfully tested, without retaining any side-effects of the callees.

Replaces
```rs
let existing = map.insert(Id(123), 0);
```
with
```rs
let existing: Option<usize> = Default::default();
```

## `call_value_default_shadow`

Replace the return value of function calls with `Default::default()` to test whether the return values of inner calls are meaningfully tested, while retaining expected side-effects of the callees.

Replaces
```rs
let existing = map.insert(Id(123), 0);
```
with
```rs
let existing: Option<usize> = {
    let _existing = map.insert(Id(123), 0);
    Default::default()
};
```

## `continue_break_swap`

Swap continue expressions for break expressions and vice versa.

Replaces
```rs
for other in mutations {
    if conflicts.contains(&(mutation, other)) { continue; }
```
with
```rs
for other in mutations {
    if conflicts.contains(&(mutation, other)) { break; }
```

## `eq_op_invert`

Invert equality checks.

Replaces
```rs
if buffer.len() == 0 {
    buffer.reserve(1024);
```
with
```rs
if buffer.len() != 0 {
    buffer.reserve(1024);
```

## `logical_op_and_or_swap`

Swap logical `&&` for logical `||` and vice versa.

Replaces
```rs
self.len() <= other.len() && self.iter().all(|v| other.contains(v))
```
with
```rs
self.len() <= other.len() || self.iter().all(|v| other.contains(v))
```

## `math_op_add_mul_swap`

Swap addition for multiplication and vice versa.

Replaces
```rs
let offset = size_of::<DeclarativeEnvironment>() * index;
```
with
```rs
let offset = size_of::<DeclarativeEnvironment>() + index;
```

## `math_op_add_sub_swap`

Swap addition for subtraction and vice versa.

Replaces
```rs
let center = Point::new(x + (width / 2), y + (height / 2));
```
with
```rs
let center = Point::new(x - (width / 2), y + (height / 2));
```

## `math_op_div_rem_swap`

Swap division for modulus and vice versa.

Replaces
```rs
let evens = 0..100.filter(|v| v % 2 == 0);
```
with
```rs
let evens = 0..100.filter(|v| v / 2 == 0);
```

## `math_op_mul_div_swap`

Swap multiplication for division and vice versa.

Replaces
```rs
let v = f64::sin(t * freq) * magnitude;
```
with
```rs
let v = f64::sin(t / freq) * magnitude;
```

## `range_limit_swap`

Invert the limits (inclusivity) of range expressions.

Replaces
```rs
for i in 0..buffer.len() {
```
with
```rs
for i in 0..=buffer.len() {
```

## `relational_op_eq_swap`

Include or remove the boundary (equality) of relational operators.

Replaces
```rs
if self.len() <= other.len() {
```
with
```rs
if self.len() < other.len() {
```

## `relational_op_invert`

Completely invert relation operators.

Replaces
```rs
while i < buffer.len() {
```
with
```rs
while i >= buffer.len() {
```
