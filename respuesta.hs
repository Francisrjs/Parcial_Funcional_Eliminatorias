module Library where
import PdePreludat
import Text.Show.Functions
import Data.Char (toUpper)
--PARTE 1, EQUIPOS
data Jugador=UnJugador {nacionalidad::String,cotizacion::Number} deriving (Eq,Show)
data Entrenador=UnEntrenador {aniosExp::Number} deriving (Eq,Show)
data Equipo= UnEquipo {entrenador::Entrenador,pais::String,jugadores::[Jugador]}deriving (Eq,Show)
scaloni= UnEntrenador 30
entrenadorDefault= UnEntrenador 10
dybala= UnJugador "Argentina" 3000
messi= UnJugador "Argentina" 300000
ney= UnJugador "Brasil" 300000
marquinhos= UnJugador "Brasil" 300000
aguero= UnJugador "Argentina" 55000
diMaria= UnJugador "Argentina" 60000
jugadorRepetido= UnJugador "China" 6
argentina= UnEquipo scaloni "Argentina" [messi,dybala,aguero]
brasil= UnEquipo scaloni "Brasil" [ney,marquinhos]
colombia= UnEquipo entrenadorDefault "Colombia" [ney,marquinhos]
china= UnEquipo entrenadorDefault "China" [jugadorRepetido,jugadorRepetido,jugadorRepetido,jugadorRepetido,jugadorRepetido,jugadorRepetido]
cantJugadoresFueraDelPais::Equipo->Number
cantJugadoresFueraDelPais equipo= (length.(filter(\jugador-> (nacionalidad jugador) == (pais equipo) )).jugadores) equipo
cotizacionJugadores::Equipo->Number
cotizacionJugadores equipo=sum(map (cotizacion) (jugadores equipo))
esMarquitinero::Equipo->Bool
esMarquitinero equipo= ((aniosExp.entrenador) equipo) >=20
cambiarEntrenador :: Equipo -> Entrenador -> Equipo
cambiarEntrenador equipo entrenadorNuevo= equipo {entrenador=entrenadorNuevo}
agregarJugador :: Equipo -> Jugador -> Equipo
agregarJugador equipo jugadorNuevo=equipo{jugadores= [jugadorNuevo]++(jugadores equipo)}
quitarUltimoJugador::Equipo->Equipo
quitarUltimoJugador equipo=equipo {jugadores= filter ((last(jugadores equipo))/=) (jugadores equipo)}
--PARTE 2, PARTIDOS (TIPOS DE ESTRATEGIAS)
--funcion para aplicar distintos casos
ganadorPartido funcion equipo1 equipo2
    |funcion equipo1 > funcion equipo2 =equipo1
    |otherwise=equipo2
--casos que se puedan dar
--Funcion 1= mejor cotizacion
promedioCotizacion::Equipo->Number
promedioCotizacion equipo= cotizacionJugadores (equipo) /length (jugadores equipo)
mejorCotizacion :: Equipo -> Equipo -> Equipo
mejorCotizacion equipo1 equipo2= ganadorPartido promedioCotizacion equipo1 equipo2
--Funcion 2= mas jugadores que juegan en su pais
cantJugadoresEnSuPais :: String -> Equipo -> Number
cantJugadoresEnSuPais paisC equipo = length(filter (\jugadores->(paisC)==(pais equipo)) (jugadores equipo))
masJugadoresEnPaisDado pais eq1 eq2=ganadorPartido (cantJugadoresEnSuPais pais) eq1 eq2
--Funcion 3= entrenador experimentado
entrenadorExperimentado :: Equipo -> Equipo -> Equipo
entrenadorExperimentado eq1 eq2= ganadorPartido (aniosExp.entrenador) eq1 eq2
--Funcion 4= inventar una nueva forma, Jugadores que tinen mas jugadores fuera del pais
fueraDelPais :: Equipo -> Equipo -> Equipo
fueraDelPais eq1 eq2=ganadorPartido (cantJugadoresFueraDelPais) eq1 eq2

--Parte 3, TIPOS DE TORNEOS
type Estrategia= Equipo->Equipo->Equipo
--Potrero
equiposP=[argentina,brasil]
potrero :: [Equipo] -> (Equipo -> Equipo -> Equipo) -> Equipo
potrero (equipo:otrosequipos) estrategia= foldl estrategia equipo otrosequipos
ganaElPartido::(Equipo->Equipo->Equipo)->Equipo->Equipo->Bool
ganaElPartido estrategia eq1 eq2=estrategia eq1 eq2 == eq2
sinElCandidato::[Equipo]->Equipo->[Equipo]
sinElCandidato equipos candidato= filter (candidato /=) equipos
leGanoAlCandidato estrategia candidato equipos= head((filter(not.ganaElPartido estrategia candidato) equipos))
leGanoATodos ::(Equipo -> Equipo -> Equipo) -> [Equipo] -> Equipo -> Bool
leGanoATodos estrategia equipos equipo= all (ganaElPartido estrategia equipo) (sinElCandidato equipos equipo)
candidato ::(Equipo -> Equipo -> Equipo) -> Equipo -> [Equipo] -> Equipo
candidato estrategia ayudador equipos
    |leGanoATodos estrategia equipos ayudador=ayudador
    |otherwise=leGanoAlCandidato estrategia ayudador equipos
invicto :: (Equipo -> Equipo -> Equipo) -> [Equipo] -> Equipo
invicto estrategia equipos
    |any(leGanoATodos estrategia equipos) equipos=head (filter(leGanoATodos estrategia equipos) equipos)
    |otherwise= head equipos
--EJEMPLOS:
--1ER EJEMPLO,Quien gana un torneo de potrero, en el que gana el que mas jugadores tiene que juegan en china, 
--participando argentina, uruguay, colombia y brasil?

--en consola:potrero [argentina,brasil,colombia,china] (masJugadoresEnPaisDado "china")
--r:UnEquipo {entrenador = UnEntrenador {aniosExp = 10}, pais = "China", jugadores = [UnJugador {nacionalidad = "China", cotizacion = 6},UnJugador {nacionalidad = "China", cotizacion = 6},UnJugador {nacionalidad = "China", cotizacion = 6},UnJugador {nacionalidad = "China", cotizacion = 6},UnJugador {nacionalidad = "China", cotizacion = 6},UnJugador {nacionalidad = "China", cotizacion = 6}]}


--2 EJEMPLO,Quien gana un torneo con candidato, donde el candidato es brasil, los partidos se ganan por mayor cotizacion de sus jugadores
--y juegan argentina, uruguay, colombia, ecuador y brasil?
--en consola: *Library> candidato mejorCotizacion brasil equiposP
--r:UnEquipo {entrenador = UnEntrenador {aniosExp = 30}, pais = "Argentina", jugadores = [UnJugador {nacionalidad = "Argentina", cotizacion = 300000},UnJugador {nacionalidad = "Argentina", cotizacion = 3000},UnJugador {nacionalidad = "Argentina", cotizacion = 55000}]}

--3 ejemplo,Quién gana un torneo en formato invicto, jugando con la estrategia inventada, con un cierto conjunto de equipos?
--en consola:*Library> invicto fueraDelPais equiposP
--r:UnEquipo {entrenador = UnEntrenador {aniosExp = 30}, pais = "Brasil", jugadores = [UnJugador {nacionalidad = "Brasil", cotizacion = 300000},UnJugador {nacionalidad = "Brasil", cotizacion = 300000}]}
--PARTE 4 - ELIMINACION DIRECTA  (FASE DE GRUPOPS)
torneo estrategia []= []
torneo estrategia (equipo:[])=[equipo]
torneo estrategia (pEquipo:sEquipo:rEquipos)=torneo estrategia ([estrategia pEquipo sEquipo]++(torneo estrategia rEquipos))
--TEORÍA:
--Para probar y pensar
--Averiguar que pasaría si todos los equipos participantes pierden al ultimo jugador en llegar 
-- ¿puede cambiar el ganador de un torneo? en caso afirmativo, mostrar un ejemplo donde suceda. En caso contrario explicar por qué no.
--RESPUESTA: cambia el resultado del torneo si la estrategia utilizada depende de todos los jugadores
--una estrategia que podría suceder en a estrategia de la cantidad de jugadoores fuera de pais o en su pais

--Justificar. ¿que pasaría si un equipo tiene infinitos jugadores?
--Debido que las mayorias las estrategias depende de los jugadores y todas las funciones que tengo son finitas
--es imposible saber cual es el ganador ya que haskell sera iomposible computarlo.
--Pero se podría ganar el torneo si y solo si con el entrenador experimentado , mientras que las otras estrategias no podría suceder ya que es infinito
