signature PPengine = PPengine
structure PPengine : PPengine
signature PPbuild = PPbuild where type 'a pptree' = 'a PPengine.pptree
structure PPbuild : PPbuild
infixr 5 ^+
infix 4 +^
infixr 4 ++
