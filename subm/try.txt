module Test where
import Cw2

-- Utils --
test_state :: State
test_state "x" = 0
test_state "y" = 0
test_state _ = 0

test_state_x_7 :: State
test_state_x_7 "x" = 0
test_state_x_7 _ = 0

test_state_x_14 :: State
test_state_x_14 "x" = 1
test_state_x_14 _ = 0

-- static: y=5; dynamic: y=6; mixed: y=10
scope_stm = parse " \
\ begin\
  \ proc p is x:=x*2; \
  \ proc q is call p; \
  \ begin var x := 5; \
    \ proc p is x := x+1; \
    \ call q;\
    \ y := x \
  \ end \
\ end"

-- x=11
recursive_stm = parse "\
\begin var x:=1; \
  \proc fac1 is begin \
    \if x<=10 then \
      \x:=x+1; \
      \call fac1 \
    \else \
      \skip \
  \end; \
  \call fac1 \
\end"


-- Even varible should be 1 if x is even else 0
-- Note load x via a state
mutal_recursion_stm = parse "\
\begin var even:=0; \
  \proc even is begin \
    \if x=0 then \
        \even:=1 \
    \else \
        \x:=x-1; \
        \call odd \
  \end; \
  \proc odd is begin \
    \if x=0 then \
        \even:=0 \
    \else \
        \x:=x-1; \
        \call even \
    \end; \
    \call even \
\end"

test1_stm = parse "\
\a := a + 1; \
\source := source + 0; \
\begin \
    \begin \
        \var b := 234 + b; \
        \var z := 7; \
        \proc p is ( \
            \d := 1234; \
            \f := z \
        \); \
        \proc pp is ( \
            \call p \
        \); \
        \( \
            \b := b + 2; \
            \begin \
                \var b := 100; \
                \var z := 3; \
                \proc p is ( \
                    \d := 9876; \
                    \f := z \
                \); \
                \call pp \
            \end \
        \) \
    \end; \
    \begin \
        \proc recursive is ( \
            \if (source <= 100) then ( \
                \source := source + 1; \
                \call recursive \
            \) else skip \
        \); \
        \call recursive \
    \end \
\end"

test_dynamic_scope = (s_dynamic scope_stm test_state) "y" == 6
test_dynamic_recusion = (s_dynamic recursive_stm test_state) "x" == 11
test_dynamic_mutal_recusion = ((s_dynamic mutal_recursion_stm test_state_x_7) "even" == 0) && ((s_dynamic mutal_recursion_stm test_state_x_14) "even" == 1)

test_mixed_scope = (s_mixed scope_stm test_state) "y" == 10
test_mixed_recusion = (s_mixed recursive_stm test_state) "x" == 11
test_mixed_mutal_recusion = ((s_mixed mutal_recursion_stm test_state_x_7) "even" == 0) && ((s_mixed mutal_recursion_stm test_state_x_14) "even" == 1)

test_static_scope = (s_static scope_stm test_state) "y" == 5
test_static_recusion = (s_static recursive_stm test_state) "x" == 11
test_static_mutal_recusion = ((s_static mutal_recursion_stm test_state_x_7) "even" == 0) && ((s_static mutal_recursion_stm test_state_x_14) "even" == 1)


test = do
  print test_dynamic_scope
  print test_dynamic_recusion
  print test_dynamic_mutal_recusion
  print test_mixed_scope
  print test_mixed_recusion
  print test_mixed_mutal_recusion
  print test_static_scope
  print test_static_recusion
  print test_static_mutal_recusion
