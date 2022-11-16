import java.util.Optional;


/*
    echo Monads.java | entr -c bash -c '~/.jdks/openjdk-17.0.2/bin/javac ./Monads.java && ~/.jdks/openjdk-17.0.2/bin/java Monads'
 */

// Good practice in Java is to use method overloading rather than require optional arguments
@SuppressWarnings("OptionalUsedAsFieldOrParameterType")
class Monads {

    public static void main(String[] args) {
        // @formatter:off
        print("add( Optional.of(1),   Optional.of(2)   )    =>  ", add( Optional.of(1),   Optional.of(2)   ));
        print("add( Optional.empty(), Optional.of(2)   )    =>  ", add( Optional.empty(), Optional.of(2)   ));
        print("add( Optional.of(1),   Optional.empty() )    =>  ", add( Optional.of(1),   Optional.empty() ));
        // @formatter:off
    }

    private static void print(String header, Optional<Integer> result) {
        System.out.printf("%s %s\n", header, result);
    }

    private static Optional<Integer> add(Optional<Integer> x, Optional<Integer> y) {
        // @formatter:off
        return x.flatMap(xx ->
               y.flatMap(yy ->
               Optional.of(xx + yy)));
        // @formatter:on
    }
}
