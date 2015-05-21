Installation:
* Ladda ned alla paket som behövs från CRAN; gdata, ggplot2, RColorBrewer, VGAM.
* Ändra variabeln ROOT i filen Load.R till installationskatalogen.
* Kör filen Load.R för att ladda programmet en första gång.

Funktioner: 
* Använd load()för att ladda om excelfilen. Detta raderar alla ändringar.
* Använd edit.outrights() för att ändra dina outrights. Om du vill spara dem behöver de läggas in i excelfilen manuellt.
* edit.result() för att modifiera matchutfall.
* Använd run() för att köra programmet med modifierade outrights.

Variabler:
* normal.sim i Main.R ändrar om normalsimulering ska användas eller ej.
* normal.nsims i Main.R ändrar antalet simuleringar med normal.
* direct.nsisms i Main.R ändrar antalet simuleringar med direkt.
* fname ändrar vilken excelfil som ska användas.

Resultat:
* winner innehåller vinnarodds.
* ranking.prob innehåller sannolikheter för att ett lag kommer på en viss plats (kanske oviktigt).
* best$Stockholm, best$West, best$Rest innehåller "Best in..."-oddsen.
* without.malmo innehåller oddsen för vinnare utan Malmö.
* relegated innehåller oddsen för relegering.
