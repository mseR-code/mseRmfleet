# A test of reading and writing.

.createList <- function( obj )
{
  # Input  a data frame with columns "parameter" and "value".
  # Output a list with elements named as parameter and values in "value".

  result <- list()

  # Shut off whining, then coerce to numeric to let NA indicate non-numerics.
  options( warn=-1 )
  numericVal <- as.numeric( obj[,"value"] )
  options( warn=0 )

  for ( i in 1:nrow(obj) )
  {
    # Value is numeric, build the parse string.
    if ( !is.na(numericVal[i]) )
      listText <- paste( "result$",obj[i,"parameter"],"=",
                    obj[i,"value"],sep="" )
    # Value is character, build the parse string.
    else
      listText <- paste( "result$",obj[i,"parameter"],"=",
                  obj[i,"value"], sep="" )

    # ARK: At one point I had this code, presumably to handle a different
    #      input format convention, perhaps assuming "value" was all character.
    #                   sQuote(obj[i,"value"]),sep="" )

    # Evaluate the parse string.
    eval( parse( text=listText ) )
  }
  result
}

# .readParFile   (reads an ASCII file with 1 comment line, header, data frame)
# Purpose:      Reads an ASCII file: 1 comment, 1 header, space-delimited
#               data frame usually containing columns "parameter" and "value".
# Parameters:   parFile is a character string indicating the input file.
# Returns:      result, a data frame.
# Source:       A.R. Kronlund
.readParFile <- function( parFile="inputFile.par" )
{
  # Read the file and store as a dataframe.
  result <- read.table( file=parFile, as.is=TRUE, header=TRUE, skip=1,
                        quote="",sep=" " )
  result
}

# .saveParFile   (saves the ref GUI parameters to a file).
# Purpose:       Saves an ASCII file with reference point parameters.
# Parameters:    Output file name for ASCII file containing ref GUI parameters.
# Returns:       NULL
# Side effects:  Writes an ASCII control file configured for read usng .readParFile.
# Source:        A.R. Kronlund
.saveParFile <- function( parObj, parFile=.FPAR, overWrite=FALSE )
{

# Uncomment this part if you want a GUI file select widget from PBSmodelling.

#  if ( !overWrite )
#  {
#    val <- NULL
    # If not overwriting the file, then use a prompt menu, bale if no selection.
#    tmpFile <- selectFile( initialfile=parFile,
#                 filetype=list( c(".par","par files") ), mode="save" )

#    if ( is.null(tmpFile) )
#      return( val )
#    else
#      parFile <- tmpFile
#  }
#  else
#  {
    # Force an overwrite of parFile.
#    cat( "\nMSG (.saveSimPars) Overwriting :",parFile,"\n" )
#  }

  cat( file=parFile, paste( "# Operating model parameters written",date(),"\n" ) )
  cat( file=parFile, "parameter value\n", append=TRUE )

  for ( i in 1:nrow(parObj) )
  {
    cat( file=parFile, parObj[i,1]," ",
                       parObj[i,2],"\n",
                       sep="", append=TRUE )
  }
  return( invisible() )
}

#------------------------------------------------------------------------------#

# NOTE: options

options( useFancyQuotes=FALSE )

# Read a par file.

fooPars <- .readParFile( parFile="mseRsim.par" )

cat( "\nWriting fooPars...\n" )
print( fooPars )

# Write the par file.

.saveParFile( fooPars, "foo.par", overWrite=TRUE )

# Read the par file back in to check it was written correctly.

fooPars2 <- .readParFile( parFile="foo.par" )

cat( "\nWriting fooPars2....\n" )
print( fooPars2 )

cat( "\n\nfooPars==fooPars2? : ",all(fooPars==fooPars2),"\n" )

# Now some checks of how to write the required formats:
# 1. numeric scalar   foo$x 42
# 2. numeric vector   foo$y c(1,2,3)
# 3. character scalar foo$a "fortyTwo"
# 4. character vector foo$b c("one","two","three")
# 5. logical scalar   foo$c TRUE
# 6. logical vector   foo$d c(TRUE,FALSE,TRUE)

foo <- list( x=42, y=c(1,2,3), a="fortyTwo", b=c("one","two","three"), c=TRUE,
             d=c(TRUE,FALSE,TRUE) )

print( foo )

# NEW FUNCTION - MAYBE CLOSE TO GENERIC.

.writeParFile <- function( listObj )
{
  # User would have to redirect output to a file, in the fashion of .saveSimPars.
  listNames <- names(listObj)
  
  for ( i in 1:length(listObj) )
  {
    valName <- listNames[i]
    val     <- listObj[[i]]
    
    if ( is.numeric(val) )
    {
      if ( length(val) > 1 )
      {
        val <- paste( "c(",paste(val,collapse=","),")",sep="" )
        cat( "\n",valName,val,"\n" )
      }
      else
        cat( "\n",valName,val,"\n" )
    }
    
    else if ( is.character(val) )
    {
      if ( length(val) > 1 )
      {
        val <- paste( "c(",paste(dQuote(val),collapse=","),")", sep="" )
        cat( "\n",valName,val,"\n" )
      }
      else
        cat( "\n",valName,dQuote(val),"\n" )
    }
    
    else if ( is.logical(val) )
    {
      if ( length(val) > 1 )
      {
        val <- paste( "c(",paste(val,collapse=","),")", sep="" )
        cat( "\n",valName,val,"\n" )
      }
      else
        cat( "\n",valName,val,"\n" )
    }
  }
}

cat( "\n\nWriting the list foo in the required format: \n" )
cat( "\nUser to modify to write to a file as per .saveSimPar...\n" )
.writeParFile( foo )