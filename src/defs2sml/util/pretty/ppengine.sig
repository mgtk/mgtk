structure PPengine : PPengine
structure PPbuild : PPbuild where type 'a pptree' = 'a PPengine.pptree
infixr 5 ^+
infix 4 +^
infixr 4 ++
