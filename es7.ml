(* #load "str.cma" *)

let rec int_pow x = function
| 0 -> 1
| n -> let b = int_pow x (n/2) in b * b * if n mod 2 == 0 then 1 else x

module PolishCalculator:
sig
  type twoOperandsOperator = Sum | Substruction | Product | Division | Power
  type sigleOperandOperator = Negate
  type singleOperandExpression = operand * sigleOperandOperator
  and expression = TwoOperandExpression of twoOperandExpression | SingleOperandExpression of singleOperandExpression | IntExpression of int
  and operand = IntOperand of int | ExpressionOperand of expression
  and twoOperandExpression = (operand * operand) * twoOperandsOperator
  val x: expression
  val expr_of: string -> expression
  val eval: expression -> int
end = struct
  
  type twoOperandsOperator = Sum | Substruction | Product | Division | Power
  type sigleOperandOperator = Negate
  type singleOperandExpression = operand * sigleOperandOperator
  and expression = TwoOperandExpression of twoOperandExpression | SingleOperandExpression of singleOperandExpression | IntExpression of int
  and operand = IntOperand of int | ExpressionOperand of expression
  and twoOperandExpression = (operand * operand) * twoOperandsOperator

  let stringToTwoOperanndsOperator = function
  | "+" -> Sum
  | "-" -> Substruction
  | "*" -> Product
  | "/" -> Division
  | "**" -> Power
  | _ -> Sum

  let stringToSingleOperandOperator = function
  | "-" -> Negate
  | _ -> Negate

  let stringIsNumber s = Str.string_match (Str.regexp "[0-9]+") s 0
  (* let stringIsNumber s = true *)

  let calculation v1 v2 = function
  | Sum -> v1 + v2
  | Substruction -> v1 - v2
  | Product -> v1 * v2
  | Division -> v1 / v2
  | Power -> int_pow v1 v2

  let singleOperandCalculation v = function
  | Negate -> -v
  


  let expr_of s =
    let rec expr_of list operands = 
      if(List.is_empty list) then operands else 
      let h = List.hd list in let l = List.tl list in
      if(stringIsNumber h) then expr_of l (IntOperand(int_of_string h)::operands)
      else let ops = expr_of l [] in 
        if(List.length ops > 1) then ExpressionOperand(TwoOperandExpression((List.hd ops, List.nth ops 1), stringToTwoOperanndsOperator(h)))::operands
        else ExpressionOperand(SingleOperandExpression(List.hd ops, stringToSingleOperandOperator(h)))::operands
  in let split_input = List.rev (String.split_on_char ' ' s)  
  in let ops = expr_of (List.tl(split_input)) [] 
  in
  if (List.length split_input == 1) then IntExpression(int_of_string s)
  else 
  if (List.length ops == 1) then SingleOperandExpression(List.hd ops, stringToSingleOperandOperator (List.hd split_input))
  else TwoOperandExpression((List.hd ops, List.hd (List.tl ops)), stringToTwoOperanndsOperator(List.hd split_input))

  let operandToExpression = function
  | IntOperand(n) -> IntExpression(n)
  | ExpressionOperand(e) -> e

  let rec eval = function
  | TwoOperandExpression((o1, o2), op) -> let eval1 = eval (operandToExpression o1) in let eval2 = eval (operandToExpression o2) in calculation (eval1) (eval2) op
  | SingleOperandExpression(o, op) -> singleOperandCalculation (eval(operandToExpression o)) op
  | IntExpression(n) -> n



  let operators = (IntOperand(1), IntOperand(2))
  let e: twoOperandExpression = operators, Sum
  let x = TwoOperandExpression(
    (
      ExpressionOperand(
        SingleOperandExpression(
          ExpressionOperand(
            TwoOperandExpression(
              (
                IntOperand(3),
                IntOperand(4)
              ),
              Sum
            )
          ),
          Negate
        )
      ),
      IntOperand(2)
    ),
    Product
  )
end


let v = PolishCalculator.expr_of "3 4 + - 2 *"
let main() = PolishCalculator.eval v;;
main()


