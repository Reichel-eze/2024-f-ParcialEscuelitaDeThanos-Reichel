module Library where
import PdePreludat

-- 1) Modelar Personaje, Guantelete y Universo como tipos de dato e implementar el chasquido de un universo.

data Personaje = UnPersonaje {
    edad :: Number,
    energia :: Number,
    habilidades :: [String],
    nombre :: String,
    planeta :: String
}deriving(Show, Eq)

type Habilidad = String
type Planeta = String

ironMan :: Personaje 
ironMan = UnPersonaje 30 10 ["usar rayo"] "iron man" "tierra"

drStrange :: Personaje 
drStrange = UnPersonaje 50 10 ["controlar mente"] "dr stange" "tierra"

groot :: Personaje 
groot = UnPersonaje 45 10 [] "groot" "tierra"

wolverine :: Personaje 
wolverine = UnPersonaje 50 10 ["usar garras"] "wolverine" "tierra"

viudaNegra :: Personaje 
viudaNegra = UnPersonaje 50 10 [] "viuda negra" "tierra"

data Guantelete = UnGuantelete {
    material :: String,
    gemas :: [Gema]
}deriving(Show, Eq)

guanteleCompletoDeUru :: Guantelete
guanteleCompletoDeUru = UnGuantelete "uru" [] 

type Universo = [Personaje]

universo1 :: Universo
universo1 = [ironMan, drStrange, groot, wolverine, viudaNegra]

intentarChasquido :: Guantelete -> Universo -> Universo
intentarChasquido guantelete universo
    | puedeChasquear guantelete = chasquido guantelete universo
    | otherwise                 = universo

puedeChasquear :: Guantelete -> Bool
puedeChasquear guantelete = estaCompleto guantelete && esDeUru guantelete 

estaCompleto :: Guantelete -> Bool
estaCompleto = (== 6) . length . gemas

esDeUru :: Guantelete -> Bool
esDeUru = (== "uru") . material

-- Chasquear un universo que contiene a todos sus habitantes y reducir a la mitad la cantidad de dichos personajes.
chasquido :: Guantelete -> Universo -> Universo
chasquido guantelete universo = take (length universo `div` 2) universo

-- Punto 2: (3 puntos) Resolver utilizando únicamente orden superior.
-- Saber si un universo es apto para péndex, que ocurre si alguno de los personajes que lo integran tienen menos de 45 años.

esAptoParaPendex :: Universo -> Bool
esAptoParaPendex = any esPendex

esPendex :: Personaje -> Bool
esPendex = (< 45) . edad

-- Saber la energía total de un universo que es la sumatoria de todas las energías de sus integrantes que tienen más 
-- de una habilidad.

energiaTotal :: Universo -> Number
energiaTotal = sum . map energia . filter (tieneMasDeNHabilidad 1)

tieneMasDeNHabilidad :: Number -> Personaje -> Bool
tieneMasDeNHabilidad n = (>n) . length . habilidades

-- Punto 3: (3 puntos) Implementar las gemas del infinito, evitando lógica duplicada.

-- Funciones Auxialiares

cambiarEnergia :: Number -> Personaje -> Personaje
cambiarEnergia valor personaje = personaje {energia = energia personaje + valor}

dejarSinEnergia :: Personaje -> Personaje
dejarSinEnergia personaje = personaje {energia = 0}

eliminarHabilidad :: Habilidad -> Personaje -> Personaje
eliminarHabilidad habilidad personaje = personaje {habilidades = filter (/= habilidad) (habilidades personaje)}

sacarTodasLasHabilidades :: Personaje -> Personaje
sacarTodasLasHabilidades personaje = personaje {habilidades = []}

cambiarPlaneta :: Planeta -> Personaje -> Personaje
cambiarPlaneta nuevoPlaneta personaje = personaje {planeta = nuevoPlaneta}

type Gema = Personaje -> Personaje

-- La mente que tiene la habilidad de debilitar la energía de un usuario en un valor dado.

mente :: Number -> Gema
mente valor = cambiarEnergia (-valor)

-- El alma puede controlar el alma de nuestro oponente permitiéndole eliminar una habilidad en particular si es que la posee. 
-- Además le quita 10 puntos de energía. 

alma :: Habilidad -> Gema
alma habilidad = cambiarEnergia (-10) . eliminarHabilidad habilidad

-- El espacio que permite transportar al rival al planeta x (el que usted decida) y resta 20 puntos de energía.

espacio :: Planeta -> Gema
espacio planeta = cambiarEnergia (-20) . cambiarPlaneta planeta

-- El poder deja sin energía al rival y si tiene 2 habilidades o menos se las quita 
-- (en caso contrario no le saca ninguna habilidad).

poder :: Gema
poder = sacarHabilidadesSegun . dejarSinEnergia

sacarHabilidadesSegun :: Personaje -> Personaje
sacarHabilidadesSegun personaje 
    | ((<= 2) . length . habilidades) personaje = sacarTodasLasHabilidades personaje
    | otherwise                                 = personaje

-- El tiempo que reduce a la mitad la edad de su oponente pero como no está permitido pelear con menores, 
-- no puede dejar la edad del oponente con menos de 18 años. 
-- Considerar la mitad entera, por ej: si el oponente tiene 50 años, le quedarán 25. 
-- Si tiene 45, le quedarán 22 (por división entera). 
-- Si tiene 30 años, le deben quedar 18 en lugar de 15. 
-- También resta 50 puntos de energía

tiempo :: Gema 
tiempo = cambiarEnergia (-50) . reducirEdadALaMitad

reducirEdadALaMitad :: Personaje -> Personaje
reducirEdadALaMitad personaje = personaje {edad = max 18 (edad personaje `div` 2)}

-- La gema loca que permite manipular el poder de una gema y la ejecuta 2 veces contra un rival.

gemaLoca :: Gema -> Gema 
gemaLoca gema = gema . gema 

-- Punto 4: (1 punto) Dar un ejemplo de un guantelete de goma con las gemas tiempo, alma que quita la habilidad 
-- de “usar Mjolnir” y la gema loca que manipula el poder del alma tratando de eliminar la “programación en Haskell”.

guanteleDeGoma :: Guantelete
guanteleDeGoma = UnGuantelete "goma" [tiempo, alma "usar Mjolnir", gemaLoca (alma "programacion en Haskell")]

-- Punto 5: (2 puntos). No se puede utilizar recursividad. Generar la función utilizar  
-- que dado una lista de gemas y un enemigo ejecuta el poder de cada una de las gemas que lo componen 
-- contra el personaje dado. Indicar cómo se produce el “efecto de lado” sobre la víctima.

utilizar :: [Gema] -> Personaje -> Personaje
utilizar gemas personaje = foldr aplicarGema personaje gemas

aplicarGema :: Gema -> Personaje -> Personaje
aplicarGema gema = gema 

-- Punto 6: (2 puntos). Resolver utilizando recursividad. Definir la función gemaMasPoderosa que dado un guantelete 
-- y una persona obtiene la gema del infinito que produce la pérdida más grande de energía sobre la víctima. 

gemaMasPoderosaV2 :: Guantelete -> Personaje -> Gema
gemaMasPoderosaV2 guantelete personaje = foldl1 (mayorPerdidaDeEnergiaV2 personaje) (gemas guantelete)

mayorPerdidaDeEnergiaV2 :: Personaje -> Gema -> Gema -> Gema
mayorPerdidaDeEnergiaV2 personaje gema1 gema2
    | (energia . gema1) personaje < (energia . gema2) personaje = gema1
    | otherwise                                                 = gema2

gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa guantelete personaje = mayorPerdidaDeEnergia personaje (gemas guantelete)

mayorPerdidaDeEnergia :: Personaje -> [Gema] -> Gema
mayorPerdidaDeEnergia _ [gema] = gema -- cuando es la unica que quedo (CASO BASE Y DE CORTE)
mayorPerdidaDeEnergia personaje (gema1:gema2:gemas)
    | (energia . gema1) personaje < (energia . gema2) personaje = mayorPerdidaDeEnergia personaje (gema1:gemas)
    | otherwise                                                 = mayorPerdidaDeEnergia personaje (gema2:gemas)

-- Punto 7: (1 punto) Dada la función generadora de gemas y un guantelete de locos:
infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = UnGuantelete "vesconite" (infinitasGemas tiempo)

--Y la función 
usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete

--Justifique si se puede ejecutar, relacionándolo con conceptos vistos en la cursada:
-- ejemplo1 = gemaMasPoderosa ironMan guanteleteDeLocos

-- No se podria llegar a un resultado porque como gemaMasPoderosa tiene que evaluar/comparar cada una de las gemas de 
-- la lista del guanteleteDeLocos para llegar a un conclusion de quien es la gema mas poderosa. Entonces al ser una 
-- lista infinita dicha recursividad no se detendria nunca porque en ningun momento me quedaria sin gemas que evaluar. 

-- ejemplo2 = usoLasTresPrimerasGemas guanteleteDeLocos ironMan

-- Este ejemplo si se prodia ejecutar porque simplemente agarra las primeras 3 gemas del guanteleteDeLocos y luego
-- se utiliza sobre el personaje (ejecuta el poder de cada una de las gemas sobre el personaje), devolviendome
-- al personaje luego de ser afectado por cada una de las gemas. Esto es posible gracias a la evaluacion perezosa (lazy
-- evaluation) con la cual trabaja Haskell que me permite realizar calculos/operar con listas infinitas sin la necesidad de
-- analizar todos los valores de la misma, permitiendo la obtencion de un resultado en este caso

-- Esta funcion si podria ejecutarse dado que aunque la lista sea infinita, no se la requiere en su completitud. Haskell al
-- al tener un motor de evalucion perezoso (lazy) primero va a entrar en la funcion (es decir, lo que hace, en este caso
-- el take 3), NO en los parametros, se va a dar cuenta que solo necesita las 3 primeras gemas de la lista infinita. Nunca
-- va a analizar la lista/estructura completa porque no necesita hacerlo!!
