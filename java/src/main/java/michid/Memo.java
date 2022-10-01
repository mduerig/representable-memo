package michid;

import static java.lang.System.currentTimeMillis;

import java.util.function.Function;
import java.util.function.Supplier;

public class Memo {

    // fibOp :: Num a => (Natural -> a) -> (Natural -> a)
    // fibOp _ 0 = 0
    // fibOp _ 1 = 1
    // fibOp v n = v (n - 1) + v (n - 2)
    public static Function<Integer, Integer> fibOp(Function<Integer, Integer> v) {
        return n -> switch (n) {
            case 0 -> 0;
            case 1 -> 1;
            default -> v.apply(n - 1) + v.apply(n - 2);
        };
    }


    // fix :: (t -> t) -> t
    // fix f = let x = f x in x
    public static <T, R> Function<T, R> fix(Function<Function<T, R>, Function<T, R>> f) {
        return new Function<T, R>() {
            @Override
            public R apply(T t) {
                return f.apply(this).apply(t);
            }
        };
    }

    // fibNaive :: Num a => Natural -> a
    // fibNaive = fix fibOp  -- same as fibRec
    public static int fibNaive(int n) {
        return fix(Memo::fibOp).apply(n);
    }

    static class Memoized<T> implements Supplier<T> {
        Supplier<T> supplier;
        T value;

        Memoized(Supplier<T> supplier) {
            this.supplier = supplier;
        }

        public T get() {
            if (value == null) {
                value = supplier.get();
            }
            return value;
        }
    }

    // data Stream a = a :> java.util.stream.Stream a
    // deriving Functor
    public record Stream<A>(A a, Memoized<Stream<A>> tail) { }

    // streamIndex :: Stream a -> (Natural -> a)
    // streamIndex (x :> _)  0 = x
    // streamIndex (_ :> xs) n = streamIndex xs (n - 1)
    public static <A> Function<Integer, A> streamIndex(Stream<A> str) {
        return n -> (n == 0)
            ? str.a
            : streamIndex(str.tail.get()).apply(n - 1);
    }

    // streamTabulate :: (Natural -> a) -> Stream a
    // streamTabulate f = fmap f naturals where
    //    naturals = 0 :> fmap (+1) naturals
    public static <A> Stream<A> streamTabulate(Function<Integer, A> f) {
        return new Stream<>(
            f.apply(0),
            new Memoized<>(() -> streamTabulate(n -> f.apply(n + 1))));
    }

    // streamMemoize :: ((Natural -> a) -> Natural -> a) -> Natural -> a
    // streamMemoize f = fix (streamIndex . streamTabulate . f)
    public static <A> Function<Integer, A> streamMemoize(Function<Function<Integer, A>, Function<Integer, A>> f) {
        // return fix(f.andThen(Memo::streamTabulate).andThen(Memo::streamIndex));
        // The above literal translation from Haskell does not work. We need to manually inline fix
        // so the streamTabulate can be memoized:

        return new Function<Integer, A>() {
            Stream<A> str = streamTabulate(f.apply(this));

            @Override
            public A apply(Integer n) {
                return streamIndex(str).apply(n);
            }
        };
    }

    // fibSmart :: Num a => Natural -> a
    // fibSmart = streamMemoize fibOp
    public static int fibSmart(int n) {
        return streamMemoize(Memo::fibOp).apply(n);
    }

    public static void main(String[] args) {
        int n = 40;

        System.out.println("Naive");
        var t0 = currentTimeMillis();
        System.out.println(fibNaive(n));
        System.out.println(currentTimeMillis() - t0);

        System.out.println("Smart");
        t0 = currentTimeMillis();
        System.out.println(fibSmart(n));
        System.out.println(currentTimeMillis() - t0);
    }

}
