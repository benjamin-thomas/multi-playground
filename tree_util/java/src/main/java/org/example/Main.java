package org.example;

import org.apache.commons.io.FileUtils;

import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;

/*
./gradlew run --args=~/code/explore/elm/turtle/

./gradlew build && java -jar build/libs/TreeUtil-1.0-SNAPSHOT.jar ~/code/explore/elm/turtle/
 */
public class Main {
    public static void main(String[] args) throws IOException {
        if (args.length < 1) {
            System.out.println("Must provide a path!");
            System.exit(1);
        }
        var path = args[0];
        walk(Paths.get(path));
    }

    private static void walk(Path root) throws IOException {
        Files.walkFileTree(root, new SimpleFileVisitor<>() {
            @Override
            public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) throws IOException {
                if (Files.isHidden(dir)) {
                    return FileVisitResult.SKIP_SUBTREE;
                }
                if (dir != root) {
                    printEntry(dir, attrs);
                }
                return super.preVisitDirectory(dir, attrs);
            }

            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
                if (!Files.isHidden(file)) {
                    printEntry(file, attrs);
                }

                return FileVisitResult.CONTINUE;
            }

            private void printEntry(Path file, BasicFileAttributes attrs) {
                boolean isDirectory = attrs.isDirectory();
                long size = attrs.size();

                Path simplePath = root.relativize(file);
                int depth = simplePath.getNameCount() - 1;
                var depthToWS = " ".repeat(depth);

                System.out.printf("[%12s] %s [%3d] %s %s\n",
                        toHuman(size), emoji(isDirectory), depth, depthToWS, simplePath);
            }

        });
    }

    private static String emoji(boolean isDirectory) {
        return isDirectory ? "📁" : "📝";
    }

    private static String toHuman(long size) {
        return FileUtils.byteCountToDisplaySize(size);
    }
}
