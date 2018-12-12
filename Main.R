# ------------------Funciones Intra----------------------

# diccionario de funciones
diccionario <- data.frame()

# Convierte de milisegundos a FPS
MsToFps <- function(ms){
    return(1000/ms)
}

# Largo cantidad de lineas
LargoDatos <- function(Datos){
    return(length(Datos))
}

# cantidad de datos 1% low
OnePercent <- function(CantidadDatos){
    return(round(CantidadDatos*0.01, digits=0))
}

# cantidad de datos 0.1% low
CeroOnePercent <- function(LargoDatos){
    return(round(LargoDatos*0.001, digits=0))
}

# Promedio de mili segundos
PromedioMs <- function(msVector){
    return(mean(msVector))
}

# Promedio del 1% low o 0.1% low
LowPromedio <- function(SortedMs,Percent){
    Sumatoria = 0
    for(i in 1:Percent){
        Sumatoria = Sumatoria + SortedMs[i]
    }
    return(Sumatoria/Percent)
}

# Fps promedio
AvgFps <- function(SortedMs){
    return(MsToFps(mean(SortedMs)))
}

# Promedio del 1% low
OneLowFps <- function(SortedMs){
    onePersent = OnePercent(LargoDatos(SortedMs))
    return(MsToFps(LowPromedio(SortedMs,onePersent)))
}

# Promedio del 0.1% low
ZeroOneLowFps <- function(SortedMs){
    CeroOnePersent = CeroOnePercent(LargoDatos(SortedMs))
    return(MsToFps(LowPromedio(SortedMs,CeroOnePersent)))
}

# Desviacion estandar
DesviacionEstandarFps <- function(SortedMs){
    return(sd(MsToFps(SortedMs)))
}
# ---------------------Funciones Inter--------------------------

AvgFps_i <- function(dataFrame){
    return(mean(dataFrame$AvgFps))
}


OneLowFpsAvg_i <- function(dataFrame){
    return(mean(dataFrame$OneLowFps))
}


ZeroOneLowFpsAvg_i <- function(dataFrame){
    return(mean(dataFrame$ZeroOneLowFps))
}
# ---------------------Configuraciones------------------------
# Ruta a el archivo a leer
ruta = "test3.csv"

# variable entre los datos (nombre de la primera carpeta)
variable = "conf 1"

# Datos en el dataframe final
resultados = "variable,AvgFps,UnoLowFps,CeroUnoLowFps"
# ------------------------------------------------------------

# argumentos dados al script
argumentos = commandArgs(trailingOnly=TRUE)

# Leer datos
Datos = read.csv(ruta,header=TRUE)

# funcion principal (toma como argumentos un vector)
Individual <- function(Argumentos,ruta){
    Datos = read.csv(ruta,header=TRUE)

    # toma todos los datos y extrae unicamente el tiempo entre cuadros
    msVector = Datos$MsBetweenPresents
    
    # Organizando datos de mayor a menor
    sortMs = rev(sort(msVector))

    # separa los argumentos en columnas
    columnas <- strsplit(Argumentos, ",")[[1]]

    # crea vector con los resultados
    resultados = c()
    for(argumento in columnas){
        resultados <- append(resultados,do.call(argumento,list(sortMs)))
    }

    return(resultados)
}

Multiples <- function(Argumentos,dataFrame){

    # separa los argumentos en columnas
    columnas <- strsplit(Argumentos, ",")[[1]]

    # crea vector con los resultados
    resultados = c()
    for(argumento in columnas){
        resultados <- append(resultados,do.call(argumento,list(dataFrame)))
    }

    return(resultados)
}

Main <- function(argumentos){

    # separador de argumentos
    for (argumento in argumentos){

        aux = strsplit(argumento,'=')
        
        if aux[1] == "path"{
            # ruta = ruta absoluta al directorio del proyecto
            ruta = aux[2]
        }

        if aux[1] == "intraArgs"{
            intraArgs = strsplit(aux[2],',')
        }


        if aux[1] == "interArgs"{
            interArgs = strsplit(aux[2],',')
        }

    }

    if (len(ruta)>0){
    # crea lista de carpetas dentro del directorio del proyecto
        carpetaPrimaria = list.files(path=ruta,full.names=FALSE,recursive=FALSE)
    }else{
        print("Error: No Path specified")
    }

    # resultadosFinales => data frame

    #for (carpeta in carpeta_primaria){
        
        # df = crear data frame con los nombres de columnas
       # for (archivo in carpeta){
            # rutaArchivo = ruta absoluta + carpeta primaria + archivo

            # df.rbind(Individual(argIndividuales,rutaArchivo)) 

        }

        # resultadosFinales.rbind(Multiples(argMultipels,df))

    }
    # print(resultadosFinales)
    


}

resul <- Individual("AvgFps,OneLowFps,ZeroOneLowFps",ruta)
print(resul)

















