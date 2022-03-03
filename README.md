# Informe Proyecto de Simulación y Programación Declarativa Agentes

## Yansaro Rodriguez Paez C-412

## Problema

El ambiente en el cual intervienen los agentes es discreto y tiene la forma de un rectángulo de N × M. El ambiente es de información completa, por tanto todos los agentes conocen toda la información sobre el agente. El ambiente puede variar aleatoriamente cada t unidades de tiempo. El valor de t es conocido.
Las acciones que realizan los agentes ocurren por turnos. En un turno, los agentes realizan sus acciones, una sola por cada agente, y modifican el medio sin que este varíe a no ser que cambie por una acción de los agentes. En el siguiente, el ambiente puede variar.Si es el momento de cambio del ambiente, ocurre primero el cambio natural del ambiente y luego la variación aleatoria. En una unidad de tiempo ocurren el turno del agente y el turno de cambio del ambiente.
Los elementos que pueden existir en el ambiente son obstáculos, suciedad, niños, el corral y los agentes que son llamados Robots de Casa. A continuación se precisan las características de los elementos del ambiente:

## Solucionando el problema

Para la modelación del problema se partió de la idea de un data Cell, que representa una casilla de nuestro ambiente:

``` haskell
data Cell = 
              Bot{pos::Position}
            | BotAgentV2{pos::Position}
            | Kid{pos::Position}
            | BotWithKid {pos::Position}
            | Dirt {pos::Position}
            | EmptyCell {pos::Position}
            | Corral {pos::Position}
            | KidInCorral {pos::Position}
            | Obstacle {pos::Position}
            deriving (Eq , Ord)
```

El tipo Cell es la unión de los otros tipos de la problemática, los cuales describen cada uno de los elementos que pueden convivir en el ambiente, y que por tanto podrían ocupar una casilla de este.
Para modelar el ambiente, comencé por una matriz de `Cell` de tamaño n x m con que simularía el ambiente de nuestro problema. Al ver que el trabajo se podía convertir un poco engorroso, se decidió crear una estructura llamada (Environmet) donde cada uno de estos objetos de nuestro ambiente convivirían aislados unos de otros. Veamos como inicializar un nuevo ambiente en el programa.

``` haskell
data Environment
  = Environment {
      bots :: [Cell],
      kids:: [Cell], 
      jails :: [Cell], 
      obstacles :: [Cell], 
      dirts :: [Cell], 
      botsWithKid :: [Cell], 
      kidsInCorral :: [Cell], 
      empties :: [Cell],
      width :: Int,
      height:: Int
  }


initEnvironment :: Int -> Int -> Environment
initEnvironment width height =
  Environment {
          width = width,
          height = height,
          bots = [],
          kids = [],
          jails = [],
          obstacles = [],
          dirts = [],
          botsWithKid = [],
          kidsInCorral = [],
          empties = [EmptyCell (n,m) | n <- [0 .. width - 1], m <- [0 .. height - 1]]
        }
  |> genCorrals |> genKids |> genObstacles |> genBots
       
```

En el código anterior, inicializamos un Environmet vacío dado un largo y ancho a tomar. Como se puede apreciar, un `Environment` vacío no es más un tipo que tiene una propiedad empties con todos las `Cell` del ambiente.

Para la simulación, cada uno de los tipos "movibles" tiene en su respectivo módulo de utilidades, una función que le permite moverse en el ambiente, validando que siempre el movimiento a ejecutar sea válido. Veamos el código del movimiento de un niño en nuestro programa a modo de ejemplo.

```haskell
walk:: Environment -> Cell -> Environment
walk environment kid = 
        let element = getRandomPosition environment kid
            in case element of
                (EmptyCell (_,_)) -> Src.Kid.tryGoToPosition environment kid (pos element)
                (Obstacle (n,m)) -> let direction = directionToMove kid (n,m)
                                    in let envWithObstaclesMoved = Src.Obstacle.walk environment (Obstacle(n,m)) direction 
                                    in Src.Kid.tryGoToPosition envWithObstaclesMoved  kid (pos element)
                _ -> environment

walkAll:: Environment -> Environment
walkAll environment = 
    foldl Src.Kid.walk environment (kids environment)

```

`Obstacle` también tiene una función `walk`, la cual no se llama a nivel de simulación, solo es llamada por un `Kid` cuando este decide caminar en su dirección y desplazarlo. Esta función se hace recursiva, moviéndose en caso de encontrar un camino válido, y retornando el mismo `environment` en caso de no poder desplazarse más. En ese caso, el `Kid` que se intento mover en esa posición se mantiene en su mismo lugar.

## Modelos de Agentes considerados

En este programa, se consideraron 2 tipos de agentes, nombrados `Bot` y `BotAgentV2`. Ambos agentes son agentes reactivos, los cuales no tienen en consideración un historial en la simulación para ejecutar su próximo movimiento.

### BotAgentV2

`BotAgentV2` es un agente en nuestro ambiente que funciona bajo la siguiente heurística:

1. Va hacia la suciedad más cercana y la limpia en caso de existir
2. En caso de encontrarse un niño en el trayecto lo recoge y prosigue a guardarlo en el corral más cercano
3. En caso de no existir suciedad accesible desde él, decide buscar al niño más cercano para llevarlo al corral más cercano.
4. En caso de no encontrar niño o suciedad accesible, espera un turno en su misma posición.

### Bot

`Bot` es un agente en nuestro ambiente que funciona bajo la siguiente heurística:

1. Va hacia el niño más cercano y lo recoge para llevarlo al corral más cercano.
2. En caso de encontrarse una suciedad en el trayecto la limpia y prosigue a guardarlo en el corral más cercano.
3. En caso de no existir ningún niño accesible desde el, decide buscar la suciedad más cercana e ir a limpiarla.
4. En caso de no encontrar niño o suciedad accesible, espera un turno en su misma posición.

## Ideas seguidas para la implementación

Ambos agentes buscan un elemento más carcano en el ambiente y se disponen a caminar hacia él. Para ello, se desarrolló una función BFS visible en el modulo Core.Bfs el cual recibe un ambiente, una posición inicial, una final y devuelve un array de posiciones que revela uno de los caminos más cortos para alcanzar la posición. En el inicio, se tuvo pensado que esta función retornara todos los caminos mínimos de una posición a otra, pero se dificultó bastante en la parte de la implementación. La idea era crear otro agente, que se moviera hacia un niño sabiendo que camino tenía más suciedades para limpiar, ya que cuando este pasara por encima, iba a limpiarlas y disminuir la cantidad de suciedades del ambiente.

### Operador "|>"

Para este proyecto, se definió el operador de pipe(inspirado en F#), que corresponde a la sintaxis "|>", operador que fue usado en múltiples ocasiones a lo largo del proyecto para pasarle el previamente computado output a una nueva función. Se decidió usar este operador muchas veces en el código por la expresividad que ganaba. Dicho esto, no se sustituyó el operador de composición de haskell, simplemente se usaron indistintamente en lid de buscar la mejor expresividad del programa.

``` haskell
-- F# pipe operator
(|>) :: a -> (a -> b) -> b
a |> b = b a


headSafe :: [a] -> Maybe a
headSafe []     = Nothing
headSafe (x:xs) = Just x
```

### Ideas generales

También se hizo uso extenso del tipo Maybe para operaciones que podrían devolver diferentes opciones, para luego hacer pattern matching usando la sintaxtis de case. Por ejemplo, veamos el código de cuando un robot intenta moverse hacia una posición.

```haskell

tryGoToPosition:: Environment -> Cell -> Position -> Environment
tryGoToPosition environment bot position = let element = getElementAtPosition environment position in
    let isKidInCorralInPosition = any ((\ a -> a == pos bot) . pos) (kidsInCorral environment)
    in case element of
        Nothing -> environment
        Just element -> 
            case (element,isKidInCorralInPosition) of
                (EmptyCell(_,_),True) -> environment {
                        empties = removeItem element (empties environment),
                        bots = removeItem bot (bots environment) ++ [ Bot (pos element)]
                    }
                (EmptyCell(_,_),False) -> environment {
                        empties = removeItem element (empties environment) ++ [ EmptyCell (pos bot)],
                        bots = removeItem bot (bots environment) ++ [ Bot (pos element)]
                    }
                (Kid(_,_),True) -> environment {
                        empties = removeItem element (empties environment),
                        kids = removeItem element (kids environment),
                        bots = removeItem bot (bots environment),
                        botsWithKid = botsWithKid environment ++ [BotWithKid(pos element)]
                    }
                (Kid(_,_),False) -> environment {
                        empties = removeItem element (empties environment) ++ [ EmptyCell (pos bot) ],
                        kids = removeItem element (kids environment),
                        bots = removeItem bot (bots environment),
                        botsWithKid = botsWithKid environment ++ [BotWithKid(pos element)]
                    }
                (Dirt(_,_),True) -> environment {
                        empties = removeItem element (empties environment),
                        dirts = removeItem element (dirts environment),
                        bots = removeItem bot (bots environment)  ++ [ Bot( pos element ) ]
                    }
                (Dirt(_,_),False) -> environment {
                        empties = removeItem element (empties environment) ++ [ EmptyCell (pos bot) ],
                        dirts = removeItem element (dirts environment),
                        bots = removeItem bot (bots environment)  ++ [ Bot( pos element ) ]
                    }
                _ -> environment

```

## Link

La implementación de este proyecto se encuentra disponible en <este repositorio> [https://github.com/yarn-rp/Simulation-haskell].

## Resultados y consideraciones

Ambos agentes se comportaron de una manera bien similar a pesar de que a priori pudiésemos pensar que `Bot` debía obtener mayor ganancia que `BotAgentV2`, ya que la acción que este desarrolla parece ser más beneficiosa. Si bien es cierto, que en pruebas donde los intervalos de variación aleatoria eran mayores, `Bot` parecía tomar ventaja sobre `BotAgentV2`, en cuanto las variaciones alcanzaban números cercanos a los descritos en el escenario, su comportamiento se hacia cada vez mas similar.

Los resultados de las simulaciones hechas con `BotAgentV2` sobre el ambiente, resultaron en una media 65.73 de suciedad final con una desviación estandart de 33.54. Esta simulación fue efectuada sobre un ambiente de tamaño 10 x 10, con un tiempo total de 30 turnos y con una variación aleatoria cada 10 turnos.

Los resultados de las simulaciones hechas con `Bot` sobre el ambiente, resultaron en una media 66.01 de suciedad final con una desviación estandart de 33.49. Esta simulación fue efectuada sobre un ambiente de tamaño 10 x 10, con un tiempo total de 30 turnos y con una variación aleatoria cada 10 turnos.
