/*Sagatavoja paraugu ar līdzenu 3x3 cm plakni no griezuma virspuses->apakšpuses virzienā.
Plakni 5 sek iemērca laktofenola šķīdumā, izvilka un 5 min gaidīja.
Tad krāsoto plakni skaloja ar dejonizētu ūdeni, līdz vairs nenāca nost krāsa.
Paraugus žāvēja istabas apstākļos 5 dienas.
Ar stereomikraskopu 20X palielinājumā, virspuses apgaismojumā (spožums 1, zemākais) katram paraugam uzņēma 9 attēlus, režģa veidā 4 stūros, 4 malās un 1 pa vidu.
 _____
|x x x|
|x x x|
|x x x|

Attēlus analizēja ar Fiji (imageJ)
Attēlus pārvērta hsb formātā, un mērīja laukumu attēla daļām, kuru nokrāsa (Hue) bija diapazonā no 124 līdz 234.*/


// Macro for HSB color-based area measurement.

// Ask user for a folder containing images.
dir = getDirectory("Choose a folder with images");
list = getFileList(dir);
if (list.length == 0) {
    print("No images found in the selected folder. Macro aborted.");
    return;
}

run("Clear Results");

for (i = 0; i < list.length; i++) {
    filename = list[i];
    if (endsWith(filename, ".tif") || endsWith(filename, ".jpg") || endsWith(filename, ".png")) {
        // --- Step 1: Open image and create HSB stack ---
        open(dir + filename);
        
        // --- Step 2: Split the image into separate HSB channels ---
        run("HSB Stack");
        run("Stack to Images");
        
        // --- Step 3: Apply threshold to Hue ---
        selectWindow("Hue");
        setThreshold(124, 234); // Apply Hue threshold
    
        // --- Step 4: Measure and store results ---
        run("Measure");
        measuredArea = getResult("Area", nResults - 1);
        setResult("Image Name", i, filename);
        setResult("Measured Area", i, measuredArea);
        
        // --- Step 5: Close everything for the next loop ---
        close("*");
    }
}
updateResults();