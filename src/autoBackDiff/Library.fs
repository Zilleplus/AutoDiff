namespace autoBackDiff

module Types =
    type BinaryOperations =
        | Add
        | Mul
        | Equal

    type UnitaryOperations =
        | Sin
        | Cos 
        | Minus 

    type Node =
        | None
        | Variable
        | Constant
        | UnitaryNode of Node*UnitaryOperations
        | BinaryNode of Node*Node*BinaryOperations

    type equation = {left: Node; right: Node }

    // define the basic operations on nodes
    let minus node = Node.UnitaryNode (node,UnitaryOperations.Minus)

    let add nodeLeft nodeRight =  match (nodeLeft,nodeRight) with
        | (None,_) -> nodeRight
        | (_,left) -> nodeLeft
        | _ ->  Node.BinaryNode (nodeLeft, nodeRight, BinaryOperations.Add)

    let mul nodeLeft nodeRight =  match (nodeLeft,nodeRight) with
        | (None,_) -> None
        | (_,None) -> None
        | ( None, None) -> None
        | _ -> Node.BinaryNode (nodeLeft, nodeRight, BinaryOperations.Mul)

    let Cos node = match node with
        | None -> None
        | _ -> Node.UnitaryNode (node,UnitaryOperations.Cos)

    let Sin node = Node.UnitaryNode (node,UnitaryOperations.Sin)

    // get computational tree simple gradient using chain rule (symbolic diff)
    // let rec gradientChainRule expression = match expression with
    //     | None -> Node.None // gradient of nothing is nothing
    //     | Variable -> Node.Constant // we do not support powers at this moment
    //     | Constant -> Node.None
    //     | UnitaryNode (node,operation)-> match operation with
    //         | UnitaryOperations.Cos -> minus (Sin (gradientChainRule node) )
    //         | UnitaryOperations.Sin -> Cos (gradientChainRule node)
    //         | UnitaryOperations.Minus -> minus (gradientChainRule node)
    //     | BinaryNode (nodeLeft,nodeRight,operation) -> match operation with
    //         | BinaryOperations.Add -> add (gradientChainRule nodeLeft) (gradientChainRule nodeRight)
    //         | BinaryOperations.Mul -> add (mul (gradientChainRule nodeLeft) nodeRight) (mul (gradientChainRule nodeRight) nodeLeft)

    // get computational tree back auto differentiation
    let rec gradientAB expression computationalTree = 
        let mulWithExpression node = mul node computationalTree
        match expression with 
            | None -> Seq.empty // gradient of nothing is nothing
            | Variable -> Seq.singleton ({left= Variable; right=computationalTree})
            | Constant -> Seq.empty
            // ----------- recursive stuff ------------- //
            | UnitaryNode (node,operation)-> match operation with
                | UnitaryOperations.Cos -> 
                    let derivative = mulWithExpression (minus (Sin (node) ))
                    gradientAB node derivative
                | UnitaryOperations.Sin -> 
                    let derivative = mulWithExpression(Cos (node))
                    gradientAB node derivative
                | UnitaryOperations.Minus -> 
                    let derivative = mulWithExpression (minus (node))
                    gradientAB node derivative
            | BinaryNode (nodeLeft,nodeRight,operation) -> match operation with
                | BinaryOperations.Mul -> 
                    let derivative left right = mulWithExpression (mul left right) 
                    let leftResult = gradientAB nodeLeft computationalTree
                    let rightResult = gradientAB nodeRight computationalTree
                    Seq.append leftResult rightResult
                | BinaryOperations.Add -> 
                    let leftResult = gradientAB  nodeLeft (mul computationalTree nodeRight)
                    let rightResult = gradientAB nodeRight (mul computationalTree nodeLeft) 
                    Seq.append leftResult rightResult
                     
    // returns the coomputational tree of the gradient calculated 
    //   using automatic backward differentiation
    // let reverseGradientTree expression = 
    //     gradientAB expression ([|Constant|] |> Seq.cast)

    // try x*cos(x) + c
    let demo1 = add (mul Variable (Cos Variable))  Constant

    // expect 1*cos(x) + x*(-sin(x)) 
    let test = (gradientAB demo1  Node.Constant ) |> Seq.toList

    
    //type UnitaryOperation = {type:Operations;evaluate:(seq<double>->seq<double>);}
    //type BinaryOperation =  {type:Operations;evaluate:(seq<double>->seq<double>->seq<double>);}

    //let cosine : UnitaryOperation = 
    //    {name=Operations.Cos;evaluate=fun x ->( x |> Seq.map cos)}
    //let sine : UnitaryOperation = 
    //    {name=Operations.Sin;evaluate=fun x ->(x |> Seq.map sin)}

    //let minus : UnitaryOperation = {name=Operations.Minus;evaluate=( Seq.map (fun x -> -x ) ) }
    
    //let zipAndApply x y functor = (Seq.zip x y) |> Seq.map functor

    //let mullElements x y = zipAndApply x y (fun (a,b) ->a*b)
    //let mull = {name=Operations.Mull;evaluate=mullElements}

    //let addElements x y = zipAndApply x y (fun (a,b) ->a+b)
    //let add = {name=Operations.Add;evaluate=addElements}

    
            
        //| BinaryExpression (nodeLeft,nodeRight,operation)-> expression