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

# ------------------------------------------------------------


Individual <- function(Argumentos,ruta){
    Datos = read.csv(ruta,header=TRUE)
    # toma todos los datos y extrae unicamente el tiempo entre cuadros
    msVector = Datos$MsBetweenPresents
    
    # Organizando datos de mayor a menor
    sortMs = rev(sort(msVector))

    # separa los argumentos en columnas
    columnas <- Argumentos

    # crea vector con los resultados
    resultados = c()
    for(argumento in columnas){
        resultados <- append(resultados,round(do.call(argumento,list(sortMs)),1))
    }

    return(resultados)
}

Multiples <- function(Argumentos,dataFrame){
    # separa los argumentos en columnas
    columnas <- Argumentos

    # crea vector con los resultados
    resultados = c()
    for(argumento in columnas){
        resultados <- append(resultados,round(do.call(argumento,list(dataFrame)),1))
    }

    return(resultados)
}

Main <- function(argumentos){
    # separador de argumentos
    
    for (argumento in argumentos){

        aux = unlist(strsplit(argumento,'='))

        if (aux[1] == "path"){
            # ruta = ruta absoluta al directorio del proyecto
            ruta = aux[2]
        }

        if (aux[1] == "intraArgs"){
            intraArgs = unlist(strsplit(aux[2], ",",fixed = TRUE))
        }


        if (aux[1] == "interArgs"){
            interArgs = unlist(strsplit(aux[2], ",",fixed = TRUE))
        }

    }

    if (length(ruta)>0){
    # crea lista de carpetas dentro del directorio del proyecto
        carpetaPrimaria = list.files(path=ruta,full.names=FALSE,recursive=FALSE)
    }else{
        print("Error: No Path specified")
    }

    titulosColumnas = c(intraArgs,interArgs)

    #df[nrow(df)+1,] <- #vector a añadir
    colnamesResultadosFinales = append(interArgs,"",after = 0)
    resultadosFinales <- as.data.frame(matrix(,0,length(colnamesResultadosFinales)))
    colnames(resultadosFinales) <- colnamesResultadosFinales

    for (carpeta in carpetaPrimaria){
        
        resultadoPorVariable <- as.data.frame(matrix(,0,length(intraArgs)))
        colnames(resultadoPorVariable) <- intraArgs
        
        archivos = list.files(path=paste(ruta,"/",carpeta,sep=""))

        for (archivo in archivos){
            if (archivo != "perf_summary.csv"){
                rutaArchivo = paste(ruta,"/",carpeta,"/",archivo,sep="")
                
                # calcula las metricas intra y las agrega al data frame resultadoPorVariable
                resultadoPorVariable[nrow(resultadoPorVariable)+1,] <- Individual(intraArgs,rutaArchivo)
            }
        }
# cambiar resultados finales, no esta añadiendo los resultados por variable
        resultadosFinales[nrow(resultadosFinales)+1,] <- append(Multiples(interArgs,resultadoPorVariable),carpeta,after = 0)

    }
    print(resultadosFinales)
    return(resultadosFinales)
    


}

argumentos = commandArgs(trailingOnly=TRUE)
argumentos = c("path=/media/david/Backup/Documentos/Benchmark/WindowsVm","intraArgs=AvgFps,OneLowFps,ZeroOneLowFps","interArgs=AvgFps_i,OneLowFpsAvg_i,ZeroOneLowFpsAvg_i")

resultados <- Main(argumentos)













