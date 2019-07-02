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
        | Variable of int
        | Constant
        | UnitaryNode of Node*UnitaryOperations
        | BinaryNode of Node*Node*BinaryOperations

    type PartialEquation = {left: Node; right: Node } 
    let unitPartialEquation = {left=One;right=One}

    // define simple print method for computational trees
    let rec serialize tree = 
        match tree with
        | None -> ""
        | Variable index -> " x{"+ sprintf "%i" index  + "}"
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

    let mergeEquations (equations :seq<PartialEquation>) =
        let addEquations (eq1 : PartialEquation) (eq2 : PartialEquation)  = 
            {eq1 with right=add eq1.right eq2.right}

        let mergeGroup varNode equations = 
            let mergedEquation = Seq.fold addEquations unitPartialEquation equations
            {left=varNode;right=mergedEquation.right}

        equations 
            |> Seq.groupBy (fun x->x.left)
            |> Seq.map (fun (v,es)  -> mergeGroup v es)

    // Get computational tree back auto differentiation comptational tree
    let rec gradientAB expression computationalTree = 
        let mulWithExpression node = mul node computationalTree
        match expression with 
        | None -> Seq.empty
        | One -> Seq.empty
        | Variable index -> Seq.singleton ({left= Variable index; right=computationalTree})
        | Constant -> Seq.empty
        | UnitaryNode (node,operation)-> 
            match operation with
            | Cos ->   gradientAB node (mulWithExpression (minus (Sin (node) )))
            | Sin ->   gradientAB node (mulWithExpression (Cos (node)))
            | Minus -> gradientAB node (mulWithExpression (minus (node)))
        | BinaryNode (nodeLeft,nodeRight,operation) -> 
            match operation with
            | Mul -> 
                let leftDerivative = gradientAB nodeLeft One
                                     |> Seq.map (fun x -> {x with right= mul x.right nodeRight})
                let rightDerivative = gradientAB nodeRight One 
                                     |> Seq.map (fun x -> {x with right= mul x.right nodeLeft})

                let mergedEquations = mergeEquations (Seq.append leftDerivative rightDerivative)
                                        |> Seq.map (fun x -> {x with right= (mulWithExpression x.right)})
                mergedEquations
            | Add -> Seq.append (gradientAB nodeLeft computationalTree) (gradientAB nodeRight computationalTree)

    // try example from wiki: x1*x2 + sin x1 
    // let demo1 = (add (mul Variable Variable)  (Sin Variable))
    // let demo1 = add (Sin Variable) Variable
    let demo1 = mul  (Variable 1) (Variable 2)
    // expected 3 equations 
    let test = (gradientAB demo1  One)  |> Seq.toList 
    let str = test |> Seq.map (fun e -> e.right) |>  Seq.map serialize  //|> Seq.map (printf "%s")
    str;;