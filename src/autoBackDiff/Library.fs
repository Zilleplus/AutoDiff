namespace Diff

module AutoDiff = 
    type BinaryOperations =
        | Add
        | Mul

    type UnitaryOperations =
        | Sin
        | Cos 
        | Minus 

    type Node =
        | None
        | One
        | Variable
        | Constant
        | UnitaryNode of Node*UnitaryOperations
        | BinaryNode of Node*Node*BinaryOperations

    type PatialEquation = {left: Node; right: Node } 

    // define simple print method for computational trees
    let rec serialize tree = 
        match tree with
        | None -> ""
        | Variable -> " x"
        | Constant -> " c"
        | One -> "1"
        | UnitaryNode (node,operation)-> 
            match operation with
            | Cos -> " cos(" + serialize node + ")"
            | Sin ->  " sin(" + serialize node + ")"
            | Minus -> " -(" + serialize node + ")"
        | BinaryNode (nodeLeft,nodeRight,operation) -> 
            match operation with
            | Mul -> "(" + serialize nodeLeft + "*" + serialize nodeRight + ")"
            | Add -> "(" + serialize nodeLeft + "+" + serialize nodeRight + ")"


    // define the basic operations on nodes
    let minus node = Node.UnitaryNode (node,UnitaryOperations.Minus)

    let add nodeLeft nodeRight =   Node.BinaryNode (nodeLeft, nodeRight, BinaryOperations.Add)

    let mul nodeLeft nodeRight =  
        match (nodeLeft,nodeRight) with
        | (None,_) -> None
        | (_,None) -> None
        | _ -> Node.BinaryNode (nodeLeft, nodeRight, BinaryOperations.Mul)

    let Cos node = 
        match node with
        | None -> None
        | _ -> Node.UnitaryNode (node,UnitaryOperations.Cos)

    let Sin node = Node.UnitaryNode (node,UnitaryOperations.Sin)

    // Get computational tree back auto differentiation comptational tree
    let rec gradientAB expression computationalTree = 
        let mulWithExpression node = mul node computationalTree
        match expression with 
            | None -> Seq.empty
            | One -> Seq.empty
            | Variable -> Seq.singleton ({left= Variable; right=computationalTree})
            | Constant -> Seq.empty
            | UnitaryNode (node,operation)-> 
                match operation with
                | Cos ->   gradientAB node (mulWithExpression (minus (Sin (node) )))
                | Sin ->   gradientAB node (mulWithExpression (Cos (node)))
                | Minus -> gradientAB node (mulWithExpression (minus (node)))
            | BinaryNode (nodeLeft,nodeRight,operation) -> 
                match operation with
                | Mul -> 
                    let leftDerivative = gradientAB nodeLeft computationalTree 
                                        |> Seq.map (fun x -> {x with right= (mulWithExpression x.right)})
                    let rightDerivative = gradientAB nodeRight computationalTree 
                                        |> Seq.map (fun x -> {x with right= (mulWithExpression x.right)})
                    Seq.append leftDerivative rightDerivative
                | Add -> Seq.append (gradientAB nodeLeft computationalTree) (gradientAB nodeRight computationalTree)

    // try example from wiki: x1*x2 + sin x1 
    // let demo1 = (add (mul Variable Variable)  (Sin Variable))
    // let demo1 = add (Sin Variable) Variable
    let demo1 = mul  Variable Variable
    // expected 3 equations 
    let test = (gradientAB demo1  One)  |> Seq.toList 
    let str = test |> Seq.map (fun e -> e.right) |>  Seq.map serialize  //|> Seq.map (printf "%s")
    str;;