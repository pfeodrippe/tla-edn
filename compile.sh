# cp ~/dev/recife/classes/recife/RecifeEdnValue.class \
#   ~/dev/tlaplus/tlatools/org.lamport.tlatools/lib/recife/RecifeEdnValue.class

cd /Users/paulo.feodrippe/dev/tlaplus/tlatools/org.lamport.tlatools
ant -f customBuild.xml -Dtest.skip=true
cd -

cp /Users/paulo.feodrippe/dev/tlaplus/tlatools/org.lamport.tlatools/dist/tla2tools.jar jars/tla2tools.jar
make build
make deploy
