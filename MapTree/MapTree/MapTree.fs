namespace MapTree

module MapTree =

    type Tree<'a> = 
        | Tree of 'a * Tree<'a> * Tree<'a>
        | Leaf of 'a
    
    let rec mapTree tree f = 
        match tree with
        | Tree (x, l, r) -> Tree ( f x, mapTree l f, mapTree r f)
        | Leaf x -> Leaf (f x)