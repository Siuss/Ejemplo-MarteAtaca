-- Defino tipos de datos, creo tres ejemplos de planetas y tres ejemplos de enfermedades

type TasaMortalidad = Float
type MedidaCombate = String
type Continente = String
type CantidadPoblacion = Int  
type Victimas = Int
type Mortalidad = Float

data Enfermedad = UnaEnfermedad {
    tasaDeMortalidad::TasaMortalidad,
    medidaDeCombate::MedidaCombate
} deriving Show

data Planeta = Planeta {
    poblacionTotal::CantidadPoblacion,
    continentes::[Continente],
    medidasTomadas::[MedidaCombate]
} deriving Show

-- Ejemplo de Enfermedades

coronaVirus :: Enfermedad
coronaVirus = UnaEnfermedad 3.5 "cuarentena"

dengue :: Enfermedad
dengue = UnaEnfermedad 2.7 "Evitar la acumulacion de agua"

peste :: Enfermedad
peste = UnaEnfermedad 1 "lavarse las manos"

-- Ejemplos de Planetas

tierraAntesDeCuarentena :: Planeta
tierraAntesDeCuarentena = Planeta 7777382699 ["America", "Asia", "Africa", "Europa", "Oceania", "Antartida"] ["lavarse las manos"]

marte :: Planeta
marte = Planeta 2 [] ["cuarentena","lavarse las manos","Evitar la acumulacion de agua"]

venus :: Planeta
venus = Planeta 15 ["Comuna 1","Comuna 2","Comuna 3","Comuna 4","Comuna 5"] ["cuarentena","evitar lugares cerrados","usar barbijo","lavarse las manos"]

-- Punto 1: 

combinarEnfermedades:: Enfermedad -> Enfermedad -> Enfermedad
combinarEnfermedades enfermedad1 enfermedad2 = UnaEnfermedad (tasaDeMortalidad enfermedad1 + tasaDeMortalidad enfermedad2) (medidaDeCombate enfermedad1)

-- Punto 2

tomarMedidasDeProteccion :: Planeta -> Enfermedad -> Planeta
tomarMedidasDeProteccion planeta enfermedad 
    | planetaProtegidoContra planeta enfermedad = planeta 
    | otherwise = implementarMedida planeta (medidaDeCombate enfermedad)

planetaProtegidoContra :: Planeta -> Enfermedad -> Bool                                            
planetaProtegidoContra planeta enfermedad = elem  (medidaDeCombate enfermedad) (medidasTomadas planeta)

implementarMedida :: Planeta -> MedidaCombate -> Planeta
implementarMedida planeta medida = planeta{medidasTomadas = medida:medidasTomadas planeta }
implementarMedida planeta medida = planeta{medidasTomadas = medidasTomadas planeta ++ [medida]}

implementarMedida (Planeta p c medidas) medida = Planeta p c (medida:medidas)
implementarMedida (Planeta p c medidas) medida = Planeta p c (medidas++[medida])


-- Punto 3

estaAlHorno :: Planeta -> Enfermedad -> Bool
estaAlHorno planeta enfermedad = not (planetaProtegidoContra planeta enfermedad) && cantidadDeVictimas planeta enfermedad > 1000000

cantidadDeVictimas :: Planeta -> Enfermedad -> Victimas
cantidadDeVictimas planeta enfermedad =   ceiling (tasaDeMortalidad enfermedad) * poblacionTotal planeta `div` 100
cantidadDeVictimas planeta enfermedad =   ceiling (tasaDeMortalidad enfermedad * fromIntegral (poblacionTotal planeta) / 100)

-- Punto 4
tieneMasMedidasQueHabitantesPorContinente :: Planeta -> Bool
tieneMasMedidasQueHabitantesPorContinente planeta = cantidadMedidasPrevencion planeta > cantidadHabitantesPorContinente planeta

cantidadMedidasPrevencion :: Planeta -> Int
cantidadMedidasPrevencion planeta = length (medidasTomadas planeta)

cantidadHabitantesPorContinente :: Planeta -> Int
cantidadHabitantesPorContinente (Planeta poblacion [] _ ) = poblacion 
cantidadHabitantesPorContinente (Planeta poblacion continentes _ ) = poblacion `div` (length continentes)

-- Punto 5
enfermedadAtacaPlaneta :: Enfermedad -> Planeta -> (Planeta,Victimas)
enfermedadAtacaPlaneta enfermedad planeta 
    | planetaProtegidoContra planeta enfermedad =  (planeta, 0)
    | otherwise = ( planetaDespuesDelAtaque planeta enfermedad , cantidadDeVictimas planeta enfermedad )

planetaDespuesDelAtaque :: Planeta -> Enfermedad -> Planeta
planetaDespuesDelAtaque planeta enfermedad = tomarMedidasDeProteccion ( atacarPlaneta planeta enfermedad ) enfermedad

atacarPlaneta :: Planeta -> Enfermedad -> Planeta
atacarPlaneta planeta enfermedad = planeta{ poblacionTotal = (poblacionTotal planeta) - (cantidadDeVictimas planeta enfermedad) }

-- Punto 6
 
tieneMasVictimasLaPrimeraCombinadaConLaSegunda :: Enfermedad -> Enfermedad -> Planeta -> Bool
tieneMasVictimasLaPrimeraCombinadaConLaSegunda enfermedad1 enfermedad2 planeta = 
    victimasAtaqueCombinado enfermedad1 enfermedad2 planeta > victimasAtaqueCombinado enfermedad2 enfermedad1 planeta

victimasAtaqueCombinado :: Enfermedad -> Enfermedad -> Planeta -> Victimas
victimasAtaqueCombinado enfermedad1 enfermedad2 planeta = snd ( enfermedadAtacaPlaneta (combinarEnfermedades enfermedad1 enfermedad2) planeta )
