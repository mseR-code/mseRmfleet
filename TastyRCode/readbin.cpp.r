#include <fvar.hpp>
/* program to read a binary file (using ADMB’s uistream and
uostream stream classes) of vectors of length n.
It is assumed that the size n is stored at the top of
the file. there is no information about any many vectors
are stored so we must check for an eof after each read
To use the program you type:
readbin filename
*/
void produce_comma_delimited_output(dvector& v)
{
int i1=v.indexmin();
int i2=v.indexmax();
for (int i=i1;i<=i2;i++)
{
cout << v(i) << ",";
}
cout << endl;
}
main(int argc, char * argv[])
{
if (argc < 2)
{
cerr << " Usage: progname inputfilename" << endl;
exit(1);
}
uistream uis = uistream(argv[1]);
if (!uis)
{
cerr << " Error trying to open binary input file "
<< argv[1] << endl;
exit(1);
}
int ndim;
uis >> ndim;
if (!uis)
{
cerr << " Error trying to read dimension of the vector"
" from the top of the file "
<< argv[1] << endl;
exit(1);
}
if (ndim <=0)
{
cerr << " Read invalid dimension for the vector"
" from the top of the file "
admb-project.org 2-3
<< argv[1] << " the number was " << ndim << endl;
exit(1);
}
int nswitch;
cout << " 1 to see all records" << endl
<< " 2 then after the prompts n1 and n2 to see all" << endl
<< " records between n1 and n2 inclusive" << endl
<< " 3 to see the dimension of the vector" << endl
<< " 4 to see how many vectors there are" << endl;
cin >> nswitch;
dvector rec(1,ndim);
int n1=0;
int n2=0;
int ii=0;
switch(nswitch)
{
case 2:
cout << " Put in the number for the first record you want to see"
<< endl;
cin >> n1;
cout << " Put in the number for the second record you want to see"
<< endl;
cin >> n2;
case 1:
do
{
uis >> rec;
if (uis.eof()) break;
if (!uis)
{
cerr << " Error trying to read vector number " << ii
<< " from file " << argv[1] << endl;
exit(1);
}
ii++;
if (!n1)
{
// comment out the one you don’t want
//cout << rec << endl;
produce_comma_delimited_output(rec);
}
else
{
if (n1<=ii && ii<=n2)
{
// comment out the one you don’t want
//cout << rec << endl;
produce_comma_delimited_output(rec);
}
}
}
while (1);
break;
case 4:
do
{
uis >> rec;
2-4 AD Model Builder
if (uis.eof()) break;
if (!uis)
{
cerr << " Error trying to read vector number " << ii
<< " from file " << argv[1] << endl;
exit(1);
}
ii++;
}
while (1);
cout << " There are " << ii << " vectors" << endl;
break;
case 3:
cout << " Dimension = " << ndim << endl;
default:
;
}
}