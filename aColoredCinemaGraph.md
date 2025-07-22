```mermaid
graph TD
  A(("A (actor)")):::t_actor
  B(("B (director)")):::t_director
  C(("C (movie)")):::t_movie
  C((C))
  D(("D (book)")):::t_book
  A ---|worked_with| B
  A --- C
  C ---|based_on| D
classDef t_actor fill:#fdf6e3;
classDef t_director fill:#eee8d5;
classDef t_movie fill:#cb4b16;
classDef t_book fill:#b58900;
classDef t_default fill:#eee;
```
