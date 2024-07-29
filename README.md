# Text mining
Dette repository indeholder digitale værktøjer til text mining, og er målrettet studerende uden eller med begrænset kendskab til programmering. Værktøjerne er udarbejdet som Shiny Apps, så brugerne kan benytte sig af dem via et genkendeligt interface uden at skulle konfronteres med den bagvedliggende kode. Værktøjerne kan derved ses som en indgangsvinkel for studerende, der ønsker at prøve kræfter med text mining. 

I denne version af værktøjet kan studerende mv. lære at bruge nogle af de forskellige funktioner, der er kendeteget ved text mining samt stifte bekendskab til forskellige visualiseringer, der relaterer sig hertil. Formålet med dette er at afmystificere text mining som analysetilgang og -værktøj, hvor visualiseringer og genkendelighed er i hovedsædet. I corpora mappen findes en række pre-klargjorte datasæt, såkaldte corpora, der alle indeholder en række tekster indenfor forskellige kategorier. Heriblandt eventyr af H.C Andersen og Brødrende Grimm samt romaner af Jane Austen. Her åbner de forskellige corpora op for forskellige analyser, hvor elementer som oversættelse mellem sprog, udvikling over tid, genre mm. kan spille ind. Her kan brugeren let skifte mellem de forskellige copora og tekster samt mellem de forskellige analyseredskaber og visualiseringer. Vi anbefaler at studerende mv. anvender de pre-klargjorte datasæt til at blive fortrolige med materialet og funktionerne, inden de selv prøver kræfter med værktøjet på deres eget data. Studerende mv. kan selv uploade en enkelt fil eller flere filer, der derefter fungerer som corpus til text mining. Her vil brugerne kunne anvende værktøjet til hurtigt at skabe overblik over en eller flere tekster i et corpus. Dette kan være en indgangsvinkel til videre analyse eller på anden vis fungere som et eksplorativt værktøj, der er med til at bidrage til nye måder at anskue tekster og corpora på. 

## Corpora
I denne mappe findes en række data, der relaterer sig til Text Mining. Hvert corpus stammer fra Det Kgl. Bibliotek eller Project Gutenberg og er uden for ophavsret. Se mere om Project Gutenberg her: https://www.gutenberg.org/ 

## Sådan tilgås materialet 
### 1. Installer R
Download den nyeste version af R ned på din computer. Husk at vælg en version, der passer til din computers styresystem. R er et ’sprog’ vi skal bruge til at programmere med.
<br> R kan downloades her: https://posit.co/download/rstudio-desktop/
### 2. Installer RStudio
Download den nyeste version af RStudio ned på din computer. Husk at vælg en version, der passer til din computers styresystem. RStudio er selve applikationen, hvori vi skriver vores kode. Det er RStudio vi åbner, når vi skal programmere.
<br> RStudio kan downloades her: https://posit.co/download/rstudio-desktop/

### 3. Kør programmet
1. Lav et nyt script under filer/files 
2. Skriv følgende kode:  
``` 
install.package("shiny")
library(shiny)
runFromGitHub("ShinyTextMining", "AUL-Arts-Digital-Lab") 
```
