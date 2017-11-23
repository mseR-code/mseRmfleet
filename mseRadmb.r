#------------------------------------------------------------------------------#
#-- ADMB Helper Functions (some hidden)                                      --#
#------------------------------------------------------------------------------#

.asIs <- function(x)
{
    if (is.numeric(x))
        x = format(x, scientific = FALSE)
    return(x)
}


.admbArgs <- function( winName=.getWinName(), ed=TRUE )
{
  getWinVal( scope = "L", winName = winName )
  p.exe <- paste( prefix, ".exe", sep = "" )
  p.arg <- paste( prefix, ".arg", sep = "" )
  p.err <- paste( "WARNING (.admbArgs): File",p.exe,"does not exist.\n", sep=" " )
  p.cmd <- paste(p.exe, "-?", sep = " ")

  if ( file.exists( p.exe ) )
    p.out <- shell( p.cmd, intern = TRUE )
  else
    p.out <- p.err

  if (ed)
  {
    writeLines( p.out, p.arg)
    .admbEdit( p.arg )
  }
  else
  {
    cat( paste( p.out, collapse = "\n") )
    cat( paste( p.arg, collapse = "\n") )
  }
  invisible(p.out)
}

.admbClean <- function( winName=.getWinName() )
{
  getWinVal( scope = "L", winName = winName )
  cleanADMB( prefix = prefix )
  invisible()
}


.admbCompile <- function ( winName=.getWinName() )
{
  #isValidOptions <- .win.checkADopts()
  isValidOptions <- TRUE
  if ( !isValidOptions )
    return()
  time0 <- proc.time()[1:3]
  getWinVal( scope = "L",winName = winName )
  safe <- !admbCk1

  compile( prefix = prefix, raneff = FALSE, safe = safe )
  Ttime = round(proc.time()[1:3] - time0, 2)
  #setWinVal( list(`Mtime[2,1]` = Ttime[1], `Mtime[2,2]` = Ttime[2],
  #                `Mtime[2,3]` = Ttime[3]), winName = winName )
  invisible( Ttime )
}

.admbEdit <- function( fname )
{
  #if ( !checkADopts(warn = FALSE))
  #{
  #  cat( "Invalid options for PBSadmb\n")
  #  stop()
  #}

  f.edit <- paste("start \"\"", .addQuotes(.convertSlashes(.admbOptions$editor)),
           .addQuotes(.convertSlashes(fname)), sep = " ")

  f.err <- paste( "WARNING (.admbEdit): File", fname, "does not exist.\n", sep=" " )
  if (file.exists(fname))
  {
    shell( f.edit, intern = TRUE )
    cat( "\nMSG (.admbEdit):", f.edit, "\n" )
    f.out <- TRUE
  }
  else
  {
    cat( f.err )
    f.out <- FALSE
  }
  return( f.out )
}

.admbEditFiles <- function( prefix, suffix = c(".dat", ".tpl", ".pin" ) )
{
  npref = length(prefix)           # Number of file prefixes.
  nsuff = length(suffix)           # Number of file extensions.
  ed.out = logical(npref * nsuff)  # Is there at least one file to edit?
  k <- 0
  for ( i in 1:npref )
  {
    for ( j in 1:nsuff)
    {
      fname <- paste( prefix[i],suffix[j], sep = "" )
      k <- k + 1
      ed.out[k] <- .admbEdit( fname )  # Returns true if a file was edited.
    }
  }
  return( ed.out )
}

.admbFindTPL <- function( suffix = ".tpl" )
{
  # The chooseWinVal call assignes the value of choice to GUI variable "prefix".
  winName <- .getWinName()               # Get the current window name
  choice = .findFileName( suffix )       # Find all files with .ext=suffix.
  chooseWinVal( choice,"prefix", winname=winName )
  invisible()
}

.admbLink <- function( winName=.getWinName() )
{
  #isValidOptions <- .win.checkADopts()
  isValidOptions <- TRUE
  if ( !isValidOptions )
    return()

  time0 <- proc.time()[1:3]
  getWinVal( scope = "L", winName = winName )

  safe <- !admbCk1
  link( prefix = prefix, raneff = FALSE, safe = safe )

  Ttime <- round( proc.time()[1:3] - time0, 2 )
  #setWinVal(list(`Mtime[3,1]` = Ttime[1], `Mtime[3,2]` = Ttime[2],
  #               `Mtime[3,3]` = Ttime[3]), winName = winName)
  invisible( Ttime )
}

.admbMake <- function( winName=.getWinName() )
{
  #isValidOptions <- .win.checkADopts()
  isValidOptions <- TRUE
  if ( !isValidOptions )
    return()

  #setWinVal(list(Mtime = matrix(NA, nrow = 3, ncol = 3)), winName = winName)
  time0 <- proc.time()[1:3]
  getWinVal( scope = "L", winName=winName )

  raneff <- FALSE
  safe <- !admbCk1

  tpl2cpp( prefix=prefix, raneff = raneff )
  Ttime <- round(proc.time()[1:3] - time0, 2)
  time0 <- proc.time()[1:3]

  #setWinVal(list(`Mtime[1,1]` = Ttime[1], `Mtime[1,2]` = Ttime[2],
  #      `Mtime[1,3]` = Ttime[3]), winName = winName)

  compile( prefix=prefix, raneff=raneff, safe=safe )
  Ttime = round( proc.time()[1:3]-time0, 2 )
  time0 = proc.time()[1:3]
  #setWinVal( list(`Mtime[2,1]` = Ttime[1], `Mtime[2,2]` = Ttime[2],
  #           `Mtime[2,3]` = Ttime[3]), winName = winName )
  link( prefix=prefix, raneff=raneff, safe=safe )
  Ttime <- round( proc.time()[1:3] - time0, 2 )
  #setWinVal( list(`Mtime[3,1]` = Ttime[1], `Mtime[3,2]` = Ttime[2],
  #           `Mtime[3,3]` = Ttime[3]), winName = winName )
  invisible()
}

.admbParseCmd <- function (prefix, index, os = .Platform$OS, comp = "GCC",
                   admpath = "", gccpath = "") 
{
  .addSlashes <- function(str) return(gsub("\\\\", "\\\\\\\\", str))
#    data(ADMBcmd)
#    dat = ADMBcmd
  dat <- ADMBcmd
  dat$OS = tolower(dat$OS)
  osdat <- dat[dat$OS %in% os & dat$Comp %in% comp, ]
  if (nrow(osdat) == 0) 
      stop("No records for specified OS and compiler")

  idat <- osdat[osdat$Index %in% index, ]
  if (nrow(idat) == 0) 
      stop("No records for specified index")
  cmd <- idat$Command[1]

  cmd <- gsub("`", "\"", cmd)
  cmd <- gsub("@prefix", .addSlashes(prefix), cmd)
  cmd <- gsub("@adHome", .addSlashes(admpath), cmd)
  cmd <- gsub("@ccPath", .addSlashes(gccpath), cmd)
  return(cmd)
}

.admbReadOptions <- function( optfile=.FADMBOPT )
{
  # Reads admb options from a file, gcc path, admb path, editor, version.
  opts <- readList( optfile )  # PBSModelling readList function.
    opts = sapply( opts, paste, collapse = " ", simplify = FALSE )
    # Not sure why this is required at the moment, was in PBSadmb.
#    for (i in names(opts))
#    {
#        setPBSoptions(i, opts[[i]])
#    }
    assign( ".admbOptions", opts, envir = .GlobalEnv )
}

.admbRun <- function( winName=.getWinName() )
{
  #setWinVal(list(Rtime = matrix(NA, nrow = 1, ncol = 3)), winName = winName)
  time0 = proc.time()[1:3]
  getWinVal(scope = "L", winName = winName)

  add     <- TRUE
  logfile <- TRUE
#  verbose <- TRUE

  run( prefix=prefix, argvec=argvec, logfile=logfile, add=add, verbose=verbose )

  if ( admbArg=="mcmc" )
  {
    p.out <- run( prefix=prefix, argvec="-mceval", logfile=logfile, add=TRUE,
                  verbose=verbose )
    writeLines( p.out, paste( prefix,".mcmc",sep="" ) )
    invisible(p.out)
  }

  Ttime = round(proc.time()[1:3] - time0, 2)
  #setWinVal(list(`Rtime[1,1]` = Ttime[1], `Rtime[1,2]` = Ttime[2],
  #          `Rtime[1,3]` = Ttime[3]), winName = winName)
  invisible( Ttime )
}

.admbSetArgs <- function( winName=.getWinName() )
{
  guiChanges <- list()
  getWinVal( scope="L", winName=winName )

  # Set normal, MCMC, or profile likelihoods.  
  if ( admbArg== "normal" )
    guiChanges$argvec <- ""
  else if ( admbArg == "mcmc" )
    guiChanges$argvec <- paste( "-mcmc", .asIs(nsims), "-mcsave", .asIs(nthin),
                                sep = " " )
  else if ( admbArg=="profile" )
    guiChanges$argvec <- "-lprof"
  
  # Set the GUI parameters.
  setWinVal( guiChanges, winName=winName )

  invisible()
}


.admbTPL2CPP <- function( winName=.getWinName() )
{
  # isOK = .win.checkADopts()
  isValidOptions <- TRUE
  if ( !isValidOptions )
  {
    cat( "\nWARNING (.admbTPL2CPP): ADMB options are invalid.\n" )
    return()
  }

  #setWinVal( list(Mtime = matrix(NA, nrow = 3, ncol = 3)), winName = winName)
  time0 = proc.time()[1:3]
  getWinVal( scope="L", winName=winName )

  # Assume no random effects.
  tpl2cpp( prefix=prefix, raneff=FALSE )

  Ttime = round(proc.time()[1:3] - time0, 2)
  #setWinVal(list(`Mtime[1,1]` = Ttime[1], `Mtime[1,2]` = Ttime[2],
  #               `Mtime[1,3]` = Ttime[3]), winName = winName)
  invisible( Ttime )
}

.admbImportRep <- function (prefix, suffix = c(".cor", ".rep", ".std", ".mc.dat"),
    global = FALSE)
{
    findFormat = function(dat) {
        for (i in 1:length(dat)) {
            if (!any(grep("^[ \t]*[#`]", dat[i]))) {
                if (any(grep("^[ \t]*structure", dat[i])))
                  fileformat = "D"
                else if (any(grep("^[ \t]*list", dat[i])))
                  fileformat = "R"
                else if (any(grep("^[ \t]*\\$", dat[i])))
                  fileformat = "P"
                else fileformat = "U"
                break
            }
        }
        return(fileformat)
    }
    sAF = options()$stringsAsFactors
    options(stringsAsFactors = FALSE)
    if (missing(prefix) || any(is.null(c(prefix, suffix))) ||
        any(c(prefix, suffix) == ""))
        return()
    flist = list()
    fname = paste(prefix, suffix, sep = "")
    if (global)
        cat("R objects created:\n")
    for (i in fname) {
        if (!file.exists(i))
            next
        mtime = file.info(i)$mtime
        if (global)
            cat(paste("     ", i, "\n", sep = ""))
        ii = substring(i, nchar(prefix) + 2)
        contents = dat = readLines(i)
        ncont = length(contents)
        ff = findFormat(contents)
        if (ff == "P")
            dat = PBSmodelling:::.readList.P(i)
        else if (ff == "D" || ff == "R")
            dat = eval(parse(i))
        else if (any(ii == c("cor", "std", "mc.dat"))) {
            tcont = gsub("std dev", "std", contents)
            if (ii == "cor")
                tcont[2] = paste("index name value std ", paste(1:(ncont -
                  2), collapse = " "))
            tfile = paste(prefix, ".tmp", sep = "")
            writeLines(tcont, tfile)
            skip = ifelse(any(ii == c("cor")), 1, 0)
            header = ifelse(any(ii == c("cor", "std")), TRUE,
                FALSE)
            fill = ifelse(any(ii == c("cor", "std")), TRUE, FALSE)
            dat = read.table(tfile, skip = skip, header = header,
                fill = fill)
            if (ii == "mc.dat") {
                report = NULL
                report = try(readList(paste(prefix, ".rep", sep = "")),
                  silent = TRUE)
                if (!is.null(report) && any(names(report) ==
                  "mcest")) {
                  if (length(report$mcest) == ncol(dat))
                    dat = rbind(report$mcest, dat)
                }
                if (!is.null(report) && any(names(report) ==
                  "mcnames")) {
                  if (length(report$mcnames) == ncol(dat))
                    names(dat) = report$mcnames
                }
            }
            if (ii == "cor") {
                ldh = as.numeric(substring(tcont[1], regexpr("=",
                  tcont[1]) + 1))
                name = NULL
                NAME = dat[, 2]
                value = dat[, 3]
                std = dat[, 4]
                names(NAME) = 1:length(NAME)
                ldupe = split(NAME, NAME)
                ndupe = sapply(ldupe, function(x) {
                  if (length(x) > 1) {
                    xnam = names(x)
                    len = length(x)
                    npad = ifelse(any(len == 10^(0:10)), 1, 0)
                    npad = ceiling(log10(len)) + npad
                    x = paste(x, pad0(1:len, npad), sep = "")
                    names(x) = xnam
                  }
                  return(x)
                }, simplify = FALSE)
                for (j in names(ndupe)) name = c(name, ndupe[[j]])
                name = name[order(as.numeric(names(name)))]
                dat = dat[, setdiff(names(dat), c("index", "name",
                  "value", "std"))]
                names(dat) = name
                row.names(dat) = name
                dat[is.na(dat)] = t(dat)[is.na(dat)]
                attr(dat, "determinant") = ldh
                attr(dat, "value") = value
                attr(dat, "std") = std
            }
        }
        if (ii != "mc.dat")
            attr(dat, "contents") = contents
        attr(dat, "mtime") = mtime
        expr = paste("if (", global, ") assign(\"", i, "\",dat,envir=.GlobalEnv); flist$",
            i, "=dat", sep = "")
        eval(parse(text = expr))
    }
    if (length(flist) == 1)
        flist = flist[[1]]
    options(stringsAsFactors = sAF)
    invisible(flist)
}


.admbViewRep <- function( winName=.getWinName() )
{
  getWinVal( scope = "L", winName=winName )
  act <- getWinAct()[1]

  if ( !is.null(act) && act == "admbView" )
  {
    if ( sum(toView) > 0 )
    {
      suffix <- paste( ".", names(toView)[toView], sep = "" )
      .admbEditFiles( prefix, suffix )
    }
  }
  invisible()
}


# NON-Hidden Helper Functions.

.appendLog <- function( prefix, lines )
{
  p.log <- paste( prefix, ".log", sep = "" )
  if ( !file.exists(p.log) )
    .startLog( prefix )
  cat( lines, file = p.log, sep = "\n", append = TRUE )
}

.callSys <- function (..., wait = TRUE) 
{
    dots = list(...)
    if (.Platform$OS.type == "windows") {
        if ("edit" %in% names(dots)) 
            dots[[1]] = paste("start \"\"", dots[[1]], sep = " ")
        out = shell(dots, intern = TRUE)
    }
    else {
        cmd <- unlist(list(...))
        if (wait == FALSE) 
            out = system(cmd, wait = FALSE)
        else out = system(cmd, intern = TRUE)
    }
    invisible(out)
}


cleanADMB <- function( prefix = NULL )
{
  ofile <- findPat( c("admodel.", "eigv.", "sims", "variance",
                    "tmp_", "fmin.log", "tfile", ".tmp", ".bak", "hessian.bin",
                    "hesscheck", "diags", "dgs2"),
                    list.files(all.files = TRUE, ignore.case = TRUE) )

  if ( is.null(prefix) )
  {
    tpl   <- .findFileName( ".tpl" )
    apat  <- paste( "^", tpl, "\\.", sep = "" )
    bpat  <- paste( "\\.", c("tpl", "dat", "pin", "r", "pdf", "mcmc"), "$", sep = "")
    afile <- sapply( apat,
               function(x) { list.files(pattern = x, ignore.case = TRUE) },
               simplify = FALSE)
    afile <- as.vector( unlist(afile) )
    bfile <- sapply( bpat,
               function(x) { list.files(pattern = x, ignore.case = TRUE) },
               simplify = FALSE)
    bfile <- as.vector( unlist(bfile) )
    pfile <- setdiff( afile, bfile )
    ofile <- union( ofile, pfile )

    if ( length(ofile) == 0 )
      ofile = NULL
    isOK <- .cleanWD( files = ofile )
    if ( !isOK )
      closeWin( "cleanWindow" )
  }
  else
  {
    pfile <- list.files(pattern=paste(prefix, "\\.", sep = ""),ignore.case=TRUE)
    if ( length(pfile) == 0 )
      psuff <- NULL
    else
    {
      psuff <- substring( pfile, nchar(prefix) + 1 )
      psuff <- setdiff( psuff,c(".tpl", ".dat", ".pin", ".r", ".pdf", ".mcmc"))
    }
    ofile <- setdiff( ofile, paste(prefix, psuff, sep = "") )
    if ( length(ofile) == 0 )
      ofile <- NULL
    isOK <- .cleanUp( prefix, suffix = psuff, files = ofile )
    if ( !isOK )
      closeWin( "cleanWindow" )
  }
}

compile <- function ( prefix, raneff = FALSE, safe = TRUE, logfile = TRUE, 
                      add = TRUE, verbose = TRUE, comp = "GCC" ) 
{
  # adp <- getOptions(.PBSadmb, "admpath")
  # gcp <- getOptions(.PBSadmb, "gccpath")
  adp <- .admbOptions$admpath
  gcp <- .admbOptions$gccpath
  index = ifelse(safe, 4, 3)
  cat( "\nMSG (compile) Compiler Path is\n", gcp, "\n\n" )
  cmd = .admbParseCmd( prefix, index = index, admpath = adp, gccpath = gcp, 
                  comp = comp )
  cat( "\nMSG (compile) Compiler Command is\n", cmd,"\n\n" )
  if (logfile & !add) 
    startLog(prefix)
  if (verbose) 
    cat(cmd, "\n")
  if ( .Platform$OS.type == "windows" )
  {
     cmd = .addQuotes(convSlashes(cmd))
  }
  
  # NOTE - THIS IS THE CALL TO THE COMPILER !!!
  out1 <- .callSys(cmd)
  
  out2 <- c(cmd, out1)
  if (logfile) 
    .appendLog(prefix, out2)
  if (verbose) 
      cat(out1, sep = "\n")
  invisible(out2)
}


link <- function( prefix, raneff = FALSE, safe = TRUE, logfile = TRUE,
                  add = TRUE, verbose = TRUE, comp="GCC" )
{
  admbPath <- .admbOptions$admpath
  gccPath  <- .admbOptions$gccpath

  if ( comp=="GCC" )
    gcc = .addQuotes( paste(gccPath, "/g++", sep = "") )
  c1 <- paste(gcc, " -o", prefix, ".exe ", prefix, ".o", sep = "")
  c2 <- paste( "-Xlinker -s -L",.addQuotes(paste(admbPath,"/lib",sep="")),sep="" )

  c3on <- "-ldf1b2stub -ladmod -ladt -lado  -ldf1b2stub -lado"
  c3sn <- "-ldf1b2stub -ladmod -ladt -lads  -ldf1b2stub -lads"
  c3or <- "-ldf1b2o -ladmod -ladt -lado -ldf1b2o -lado"
  c3sr <- "-ldf1b2s -ladmod -ladt -lads -ldf1b2s -lads"

  cmdon <- .addQuotes( paste(c1, c2, c3on, collapse = " ") )
  cmdsn <- .addQuotes( paste(c1, c2, c3sn, collapse = " ") )
  cmdor <- .addQuotes( paste(c1, c2, c3or, collapse = " ") )
  cmdsr <- .addQuotes( paste(c1, c2, c3sr, collapse = " ") )

  if ( safe )
  {
    if ( raneff )
      cmd <- cmdsr
    else cmd <- cmdsn
  }
  else
  {
    if ( raneff )
      cmd <- cmdor
    else cmd <- cmdon
  }
  if ( logfile & !add )
    .startLog( prefix )

  if ( verbose )
    cat( cmd, "\n" )

  out1 <- shell(cmd, intern = TRUE)
  out2 <- c(cmd, out1)

  if ( logfile )
    .appendLog(prefix, out2)

  if ( verbose )
    cat( out1, sep = "\n" )

  invisible( out2 )
}


.cleanUp <- function (prefix, suffix, files)
{
    if (missing(suffix))
        suffix = character(0)
    if (missing(files))
        files = character(0)
    rowLen = ceiling(sqrt(max(length(suffix), length(files))))
    if (rowLen == 0)
        return(invisible(FALSE))
    winDesc = c("window name=cleanWindow title=Clean", "grid 1 3",
        paste("entry name=cleanPrefix value=\"", prefix, "\" label=Prefix ",
            "mode=character width=12 font=\"bold 9\"", sep = ""),
        "button text=\">\" function=.win.findClean", "button text=refresh function=.cleanUpAgain",
        "label text=\"\n\nSuffixes to Clean\" font=\"bold 9\"",
        PBSmodelling:::.makeCleanVec("suff", suffix, rowLen),
        "label text=\"\n\nFiles to Clean\" font=\"bold 9\"",
        PBSmodelling:::.makeCleanVec("file", files, rowLen),
        "grid 1 3 relief=groove padx=4 pady=4", "button function=.selectCleanBoxes action=1 text=\"Select All\" padx=4 pady=4",
        "button function=.selectCleanBoxes action=0 text=\"Deselect All\" padx=4 pady=4",
        "button function=.doCleanWD text=Clean bg=aliceblue padx=4 pady=4")
    createWin(winDesc, astext = TRUE)
    invisible(TRUE)
}

.cleanWD <- function (files)
{
    rowLen = ceiling(sqrt(length(files)))
    if (rowLen == 0) {
        try(closeWin("cleanWD"), silent = TRUE)
        return(invisible(FALSE))
    }
    winDesc = c("window name=cleanWD title=Clean", "label text=\"\n\nFiles to Clean\" font=\"bold 9\"",
        PBSmodelling:::.makeCleanVec("file", files, rowLen),
        "grid 1 3 relief=groove padx=4 pady=4", "button function=.selectCleanBoxes action=1 text=\"Select All\" padx=4 pady=4",
        "button function=.selectCleanBoxes action=0 text=\"Deselect All\" padx=4 pady=4",
        "button function=doAction text=Clean bg=aliceblue padx=4 pady=4 action=\".doCleanWD(); closeWin(`cleanWD`)\"")
    createWin(winDesc, astext = TRUE)
    invisible(TRUE)
}

.doCleanWD <- function()
{
    vec = getWinVal(scope = "L")
    vecList = logical()
    for (i in names(vec)) vecList = c(vecList, vec[[i]])
    filenames = names(vecList)[vecList]
    if (!length(filenames))
        showAlert("No files to delete.")
    else if (getYes(paste("Delete ", paste(filenames, collapse = ", "),
        "?", sep = "")))
        file.remove(filenames)
    remaining = file.exists(filenames)
    if (sum(remaining))
        showAlert(paste("Failed to delete", paste(filenames[remaining],
            collapse = ", ")))
}

run <- function( prefix, argvec="", logfile=TRUE, add=TRUE, verbose=TRUE )
{
  p.exe <- paste( prefix, ".exe", sep = "" )
  if ( logfile )
  {
    p.log <- paste( prefix, ".log", sep = "" )
    p.log.log <- paste( p.log, ".log", sep = "" )
    if ( file.exists(p.log) )
      file.copy( p.log, p.log.log, overwrite = TRUE )
  }

  p.cmd <- paste( p.exe, paste(argvec, collapse = " "), sep = " " )
  p.err <- paste( "\nWARNING (run): File",p.exe,"does not exist.\n",sep=" " )
  if ( file.exists(p.exe) )
    p.out <- shell( p.cmd, intern = TRUE )
  else
    p.out <- p.err

  if ( logfile )
  {
    if ( !add )
      .startLog( prefix )
    else
      if ( file.exists( p.log.log) )
        file.copy( p.log.log, p.log, overwrite = TRUE )
    .appendLog( prefix, p.out )
  }

  if ( verbose )
    cat( p.out, sep = "\n" )
  invisible(p.out)
}


.startLog <- function( prefix )
{
  p.log <- paste(prefix, ".log", sep = "")
  if (file.exists(p.log))
    file.remove(p.log)
  dstamp <- date()
  line1 <- paste( "Log file for ", prefix, " (", dstamp, ")\n", sep = "" )
  writeLines( line1, con = p.log )
}


tpl2cpp <- function( prefix, raneff=FALSE,logfile=TRUE,add=FALSE,verbose=TRUE )
{
  admbPath <- .admbOptions$admpath
  if ( raneff )
    cmd1 <- "tpl2rem.exe"
  else
    cmd1 <- "tpl2cpp.exe"
  cmd1 = .addQuotes( paste(admbPath, "/bin/", cmd1, sep = "") )

  # Apppends path with executable string, space separated.
  cmd2 <- paste( cmd1, prefix, collapse=" " )

  # Makes a batch file for random effects models.
  if ( raneff )
  {
    .makeREbat()
    cmd2 = paste( "re", prefix, collapse = " " )
  }

  # Are we logging the output from tpl2cpp?
  if ( logfile & !add )
    .startLog( prefix )
  if ( verbose )
    cat( "\nMSG (tpl2cpp): ", cmd2, "\n" )

  # Shell out and make the output a R object.
  tplout  <- shell( cmd2,intern=TRUE )
  tplout2 <- c( cmd2, tplout )
  if ( logfile )
    .appendLog( prefix, tplout2 )
  if ( verbose )
    cat( "MSG (tpl2cpp): ",tplout, sep="\n" )

  # Clean up the random effects batch file.
  if ( raneff )
    file.remove( "re.bat" )

  invisible( tplout2 )
}