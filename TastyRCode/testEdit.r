convSlashes <- function (expr, os = .Platform$OS.type, addQuotes = FALSE)
{
    if (os == "windows")
        expr = gsub("/", "\\\\", expr)
    else expr = gsub("\\\\", "/", expr)
    if (addQuotes)
        expr = paste("\"", expr, "\"", sep = "")
    return(expr)
}


.editTextFile <- function( fName, editApp="notepad.exe" )
{
  options( "useFancyQuotes"=FALSE )

  # convSlashes is a PBSmodelling function that handles backslashes in
  # file paths.  dQuote adds double quotes, but options must be set to
  # options( "useFancyQuotes"=FALSE )
  
  # Check to see if the files exist, if they don't eliminate the missing files.
  fileExists <- logical( length(fName) )
  for ( i in 1:length(fName) )
  {
    fileExists[i] <- file.exists( fName[i] )
    if ( fileExists[i]==FALSE )
      cat( "\nWARNING (.editTextFile): File ",fName[i]," does not exist.\n" )
  }
  fName <- fName[fileExists]

  # If files exist to edit then proceed.
  if ( length(fName) > 0 )
  {
    if( .Platform$OS.type=="unix" )
    {
      fEdit  <- paste( dQuote(editApp),
                       paste(dQuote(fName),collapse=" "), sep=" " )
      system( fEdit, intern=FALSE, wait=FALSE, show.output.on.console=FALSE,
              invisible=TRUE )
    }
    else
    {
      fEdit <- paste( dQuote(editApp),
                      paste(dQuote(fName),collapse=" "), sep=" " )
      system( fEdit, intern=FALSE, wait=FALSE, show.output.on.console=FALSE,
              invisible=TRUE )
    }
    cat( "\nMSG (.editTextFile): Editing ", fEdit, "\n" )
  }
  return( invisible() )
}     # .editTextFile function

#------------------------------------------------------------------------------#

write.table( x=matrix( 1:100,nrow=10), file="test1.dat" )
write.table( x=matrix( 1:100,nrow=2 ), file="test2.dat" )

# Works on Windows.
if ( .Platform$OS.type=="windows" )
  editor <- "C:/Program Files/Tinn-R/bin/Tinn-R.exe"

if ( .Platform$OS.type=="unix" )
  editor <- getOptions( "editor" )
  
.editTextFile( fName=c("test1.dat","test2.dat"), editApp=editor )