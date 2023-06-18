package org.example.api;

import io.vavr.control.Either;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.ProtocolException;
import java.net.URL;

public class Colissimo {
    public static Either<String, byte[]> fetch(String apiKey, String tracking) {
        // @formatter:off
        return getConnection(tracking)
        .flatMap(conn   -> setVerb(conn, "GET")
        .flatMap(void1  -> setProperty(conn, "accept", "application/json")
        .flatMap(void2 -> setProperty(conn, "X-Okapi-Key", apiKey)
        .flatMap(void3 -> getResponseCode(conn))
        .flatMap(code -> {
            if (code == 200 || code == 404){

                return Either.right((Void) null);
            } else {
                return Either.left(String.format("Request returned status: %d", code));
            }
        })
        .flatMap(void4 -> getBody(conn))
        )));
        // @formatter:on
    }


    private static Either<String, byte[]> getBody(HttpURLConnection conn) {
        BufferedReader reader = null;
        try {
            reader = new BufferedReader(new InputStreamReader(conn.getInputStream()));

            StringBuilder body = new StringBuilder();
            String line;
            while ((line = reader.readLine()) != null) {
                body.append(line);
            }
            reader.close();
            return Either.right(body.toString().getBytes());

        } catch (IOException e) {
            return Either.left("Could not get HTTP body");
        }
    }

    private static Either<String, Integer> getResponseCode(HttpURLConnection conn) {
        try {
            return Either.right(conn.getResponseCode());
        } catch (IOException e) {
            return Either.left("Failed to get response code!");
        }
    }

    @SuppressWarnings("SameParameterValue")
    private static Either<String, Void> setVerb(HttpURLConnection conn, String meth) {
        try {
            conn.setRequestMethod(meth);
            return Either.right(null);
        } catch (ProtocolException e) {
            String errorMsg = String.format("Could not set verb: %s (uri=%s)", meth, conn.getURL());
            return Either.left(errorMsg);
        }
    }

    private static Either<String, Void> setProperty(HttpURLConnection conn, String key, String val) {
        try {
            conn.setRequestProperty(key, val);
            return Either.right(null);
        } catch (RuntimeException e) {
            String errorMsg = String.format("Could not set property: %s (uri=%s)", key, conn.getURL());
            return Either.left(errorMsg);
        }
    }


    private static Either<String, HttpURLConnection> getConnection(String tracking) {
        String uri = String.format("https://api.laposte.fr/suivi/v2/idships/%s?lang=fr_FR", tracking);
        try {
            HttpURLConnection conn = (HttpURLConnection) new URL(uri).openConnection();
            return Either.right(conn);
        } catch (IOException e) {
            return Either.left(String.format("Could not open a connection to: %s", uri));
        }
    }
}
