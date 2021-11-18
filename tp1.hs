import Test.HUnit


data RTE a = Rose a [(Char,RTE a)]


instance Show a => Show (RTE a) where
  show (Rose i xs) = "Rose " ++ show i ++ " " ++ show xs


--------------Resolver--------------
instance Eq a => Eq (RTE a) where       --fixme: averiguüar que es esto de instance 
    (==) (Rose a []) (Rose b []) = a == b
    (==) (Rose a as) (Rose b bs) = a == b && length as == length bs && listIsIn as bs && listIsIn bs as where
        listIsIn [] _ = True
        listIsIn _ [] = False
        listIsIn ((_, x):xs) ys =  any (\(_, y) -> x == y) ys && listIsIn xs ys

--fixme: averiguar por que a veces uso RTE y a veces Rose.
foldRose :: (a -> b -> b) -> b -> RTE a -> b  
-- foldRose toma una función (que toma un elemento de tipo a -el tipo del rose-, toma el resultado de los llamado recursivos -de tipo b-
-- y devuelve un resultado de tipo b), un caso base (de tipo b), y un Rosetree de tipo a y devuelve un resutlado de tipo b.--
foldRose f z (Rose a []) = f a z           --foldRose de la función f, sobre un RTE sin hijos, devuelve la ejecución de f sobre la raíz y el caso base z.
-- foldl ::	(a -> b -> a) -> a -> [b] -> a
-- foldlEnESteCaso ::  (b -> (Char, RTE a) -> b) -> b -> [(Char, RTE a)] -> b
foldRose f z (Rose a as) = f a (foldl (\z (_, hijo) -> foldRose f z hijo) z as)
--fixme: ver el error que me tiraba al usar foldr en vez de foldl.


mapRTE = undefined --fixme: averiguar diferencia entre map y fold.


nodos :: RTE a -> [a]
nodos = undefined


etiquetas :: RTE a -> [Char]
etiquetas = undefined


altura :: RTE a -> Int
altura = undefined


ramas :: RTE a -> [String]
ramas = undefined


subRose :: RTE a -> Int -> RTE a
subRose = undefined


tests :: IO Counts
tests = do runTestTT allTests

allTests = test [
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6
  ]

rose1 = Rose 1 [('a',Rose 2 [('c',Rose 4 [])]),('b',Rose 3 [])]
roseIgualA1 = Rose 1 [('a',Rose 2 [('c',Rose 4 [])]),('b',Rose 3 [])]
rose1DistintaRaiz = Rose 123 [('a',Rose 2 [('c',Rose 4 [])]),('b',Rose 3 [])]
rose1ConOtroOrdenDeHijos = Rose 1 [('b',Rose 3 []), ('a',Rose 2 [('c',Rose 4 [])])]
rose1PeroConMasHijos = Rose 1 [('b',Rose 3 []), ('a',Rose 2 [('c',Rose 4 []), ('d', Rose 12 [])])]


roseProfundidad1 = Rose 5 [('a',Rose 2 []), ('c',Rose 4 []), ('b',Rose 3 [])]
roseConMinEnElNivelMasProfundo = Rose 5 [('a',Rose 3 [('c',Rose 4 [])]),('b',Rose 2 [])]
roseSinHijos = Rose 3 []

--fold1 = foldRose (+) 0 rose1 --fixme: no sé si esto sería mejor ponerlo dentro del testsEj2
  
testsEj1 = test [
  rose1 ~=? roseIgualA1,
  -- rose1 ~=? rose1DistintaRaiz  	fixme: averigüar cómo hacer para que hacer un assert negado (para mostrar que dos rose son distintos).
  rose1 ~=? rose1ConOtroOrdenDeHijos
  -- rose1 ~=? rose1PeroConMasHijos	fixme: averigüar cómo hacer para que hacer un assert negado (para mostrar que dos rose son distintos).
  ]

testsEj2 = test [
  -- fixme: ver si son sufientes tests.
  -- Mínimo en la raíz:
  foldRose min 100 rose1 ~=? 1, --fixme: acá tengo un problema, porque tengo que conocer al mínimo del rose para pasarlo como caso base .
  -- Mínimo en un rose sin hijos:
  foldRose min 100 roseSinHijos ~=? 3, 
  -- Mínimo en un hijo:
  foldRose min 100 roseProfundidad1 ~=? 2,
  -- Mínimo en una hoja:
  foldRose min 100 roseConMinEnElNivelMasProfundo ~=? 2,  -- Fixme: este test muestra que no tiene problemas con llegar a la última hoja del rose,.

  -- Sumatoria de todos los valores del roseTree;
  foldRose (+) 0 rose1 ~=? 10,
  -- Productoria de todos los valores del roseTree;
  foldRose (*) 1 rose1 ~=? 24

  ]

testsEj3 = test [
  2 ~=? 1+1,
  4 ~=? 2*2
  ]

testsEj4 = test [
  2 ~=? 1+1,
  4 ~=? 2*2
  ]

testsEj5 = test [
  2 ~=? 1+1,
  4 ~=? 2*2
  ]

testsEj6 = test [
  2 ~=? 1+1,
  4 ~=? 2*2
  ]

  --}