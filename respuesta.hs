module Library where
import PdePreludat
import Text.Show.Functions
import Data.Char (toUpper)
--PARTE 1, EQUIPOS
data Jugador=UnJugador {nacionalidad::String,cotizacion::Number} deriving (Eq,Show)
data Entrenador=UnEntrenador {aniosExp::Number} deriving (Eq,Show)
data Equipo= UnEquipo {entrenador::Entrenador,pais::String,jugadores::[Jugador]}deriving (Eq,Show)
scaloni= UnEntrenador 30
dybala= UnJugador "Argentina" 3000
messi= UnJugador "Argentina" 300000
ney= UnJugador "Brasil" 300000
marquinhos= UnJugador "Brasil" 300000
aguero= UnJugador "Argentina" 55000
diMaria= UnJugador "Argentina" 60000
argentina= UnEquipo scaloni "Argentina" [messi,dybala,aguero]
brasil= UnEquipo scaloni "Argentina" [ney,marquinhos]
colombia= UnEquipo scaloni "Argentina" [ney,marquinhos]
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
--PARTE 2, PARTIDOS
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

--Parte 3, TORNEOS
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
candidato estrategia ayudador equipos
    |leGanoATodos estrategia equipos ayudador=ayudador
    |otherwise=leGanoAlCandidato estrategia ayudador equipos
invicto estrategia equipos
    |any(leGanoATodos estrategia equipos) equipos=head (filter(leGanoATodos estrategia equipos) equipos)
    |otherwise= head equipos

--PARTE 4 - ELIMINACION DIRECTA 
torneo estrategia []= []
torneo estrategia (equipo:[])=[equipo]
torneo estrategia (pEquipo:sEquipo:rEquipos)=torneo estrategia ([estrategia pEquipo sEquipo]++(torneo estrategia rEquipos))
--TEORÍA:
--Para probar y pensar
--Averiguar que pasaría si todos los equipos participantes pierden al ultimo jugador en llegar 
-- ¿puede cambiar el ganador de un torneo? en caso afirmativo, mostrar un ejemplo donde suceda. En caso contrario explicar por qué no.
--RESPUESTA: cambia el resultado del torneo si la estrategia utilizada depende de todos los jugadores

--Justificar. ¿que pasaría si un equipo tiene infinitos jugadores?
--debido que todas las estrategias depende de los jugadores y todas las funciones que tengo son finitas
--es imposible saber cual es el ganador ya que haskell sera iomposible computarlo
