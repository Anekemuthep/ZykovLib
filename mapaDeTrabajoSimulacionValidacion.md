```mermaid
graph TD
  validacionSimulacion
  metodos
  simulation
  openFOAM
  tiempoSimulado
  Parametros
  LesSmagorinsky
  MPPIC

  metodos --- validacionSimulacion
  Parametros --- metodos
  metodos --- openFOAM
  metodos --- simulation
  metodos --- tiempoSimulado
  LesSmagorinsky --- simulation
  MPPIC --- simulation

```
