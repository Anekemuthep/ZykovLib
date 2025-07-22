```mermaid
graph TD
  reunionInvestigacion
  gabriel
  calibracionTransductores
  monteCarlo
  MonteCarlo
  necesitaDatos
  yaEstanLosExperimentales
  hipotesis
  impresion3d
  mostacillas
  analisisDeImagenes

  calibracionTransductores --- reunionInvestigacion
  gabriel --- reunionInvestigacion
  gabriel --- monteCarlo
  MonteCarlo --- necesitaDatos
  MonteCarlo --- yaEstanLosExperimentales
  calibracionTransductores --- hipotesis
  analisisDeImagenes --- hipotesis
  hipotesis --- impresion3d
  hipotesis --- mostacillas

```
