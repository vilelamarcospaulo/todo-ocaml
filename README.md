# Todo (OCaml + Dream)

This project is a simple Todo application built using the OCaml programming language and the Dream web framework. It is intended to serve as a learning exercise for both languages.

## Installation

To get started, you will need to install the following dependencies:

 
[OCaml](https://ocaml.org/install) `v5.1.1`

[Opam](https://opam.ocaml.org/doc/Install.html) `Ocaml package manager` 

[Dune](https://dune.readthedocs.io/en/stable/howto/install-dune.html) 

Once you have installed the dependencies, you can clone this repository and run the following command to build the project:
  
    $ opam install ./ --deps-only
    $ dune build

 PS: it's recommended to use a local `opam switch` to avoid conflicts with other projects.
 
Usage
`$ dune exec todo`

## Testing
The project is using the default library `alcotest` for testing. To run the tests, you can use the following command:

    $ dune test

## Development
#####  Basic CRUD
 - [X] POST `/todos`
 - [ ] GET `/todos/:id`
 - [ ] DELETE `/todos/:id`
 - [ ] POST `/todos/:id/complete`

##### JWT Session - Enable Social Login
- [ ]  Google oauth2
- [ ] Apple oauth2

#####  Ownership 
- [ ] Associate each `todo` with a `owner`
- [ ] Associate a `todo` with a `@` responsible to execute it. 


The Todo application is a simple application, but it can be extended in many ways. For example, you could add support for editing todo items, due dates, and priorities. You could also add a web-based interface to the application.

  

If you are interested in extending the Todo application, I encourage you to explore the Dream framework documentation and the OCaml language documentation.

  
The Todo application is a simple but useful example of how to use the OCaml programming language and the Dream web framework. I hope you found this project helpful.

Dream framework website: https://aantron.github.io/dream/

Based from ideas in: https://fsharpforfunandprofit.com/rop/
