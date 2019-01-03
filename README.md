# XSD to GraphML Converter for yEd
It converts an xsd document into a [yEd](https://www.yworks.com/products/yed)-compatible GraphML representation.

From [this](https://github.com/zhuj/mentha-xsd-graphml/raw/master/docs/sample.xsd) into
into [this](https://github.com/zhuj/mentha-xsd-graphml/raw/master/docs/sample.xsd.graphml "The report"): 
![this](https://github.com/zhuj/mentha-xsd-graphml/raw/master/docs/sample.xsd.png "XSD GraphML Image")

ps. Node layout is still made be yEd

# Instruction

1. Use JDK either version 8 or 11 (I checked with OracleJDK8 and OpenJDK11)

1. Download the latest build [this](https://github.com/zhuj/mentha-xsd-graphml/raw/master/mentha-xsd-to-graphml-1.0.0-SNAPSHOT.zip), 

1. Or build it by yourself (use java 8/11 and gradle 5.0 or newer):
   ```
    ./gradlew assemble 
   ```    
   file will be available in build/distributions

1. Extract zip archive (do not forget make bin/mentha-xsd-to-graphml executable)
 
1. Run the tool with specified xsd file:
   ```    
    ./mentha-xsd-to-graphml-1.0.0-SNAPSHOT/bin/mentha-xsd-to-graphml docs/sample.xsd
   ```

1. Download [yEd](https://www.yworks.com/products/yed) and open generated .graphml file (should be placed next to the original xsd)

1. Navigate to menu Layout / Hierarchical, select Orientation to "Top to Bottom" and press "Ok" (you will see the magic of yFiles)

1. You can modify and/or export your schema as png/jpg/svg file to make your customer believe that things you are working on are too complicated   

