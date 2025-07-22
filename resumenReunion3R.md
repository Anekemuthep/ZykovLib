```mermaid
graph TD
  reunionInvestigacion
  gabriel
  calibracionTransductores
  MonteCarlo
  necesitaDatos
  yaEstanLosExperimentales
  hipotesis
  impresion3d
  mostacillas
  analisisDeImagenes

  calibracionTransductores --- reunionInvestigacion
  gabriel --- reunionInvestigacion
  MonteCarlo --- gabriel
  MonteCarlo --- necesitaDatos
  MonteCarlo --- yaEstanLosExperimentales
  calibracionTransductores --- hipotesis
  analisisDeImagenes --- hipotesis
  hipotesis --- impresion3d
  hipotesis --- mostacillas

```
