smlsharp <<EOF
type malt = {Brand:string, Distiller:string,
             Region:string, Age:int};
val myMalt= {Brand = "Glen Moray", 
             Distiller = "Glenlivet", 
             Region = "the Highlands", Age = 28};
{Name = {FirstName="Kenny",LastName="Drew"},Age=72};
fun createGlen'sMalt (name,age) =
    {Brand = name, Distiller = "Glenlivet",
     Region = "the Highlands", Age = age};
#Distiller myMalt;
fun oldMalt (x:{Brand:'a, Distiller:'b, Region:'c, Age:int}) =
    #Age x > 18;
oldMalt myMalt;
fun oldMalt {Brand, Distiller, Region, Age} = Age > 18;
val {Brand = brand, Distiller = distiller,
     Age = age, Region = region} = myMalt;
val {Region = r, ...} = myMalt;
val {Region, ...} = myMalt;
val distiller = (fn {Distiller,...} => Distiller) myMalt;
fun getRegion ({Region, ...}:malt) = Region;
getRegion myMalt;
val joe = {Name="Joe", Age=21};
#Name joe;
#Name {Name={First="Joe",Last="Doe"}, Office="51B"};
val getAge = #Age;
fun getRegion {Region, ...} = Region;
(getAge myMalt, getRegion myMalt);
fun incAge (r as {Age,...}) = r # {Age = Age + 1};
incAge joe;
val p = ("SML","#");
fun f (x,y) = x ^ y;
f p;
#2 p;
val {1=x,2=y} = p;
f {1="Standard ", 2="ML"};
val second = #2;
second p;
EOF
