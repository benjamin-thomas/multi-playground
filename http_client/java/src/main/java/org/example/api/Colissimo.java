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
        getConnection(tracking)
        .flatMap(conn   -> setVerb(conn, "GET")
        .flatMap(_void  -> setProperty(conn, "accept", "application/json")
        .flatMap(_void2 -> setProperty(conn, "X-Okapi-Key", apiKey)
        .flatMap(__void3 -> {
            try {
                // FIXME: I'm really going against the language at this point!
                int code = conn.getResponseCode();} catch (IOException e) {
                throw new RuntimeException(e);
}
        }))));
        // @formatter:on


        int statusCode = connection.getResponseCode();
        if (statusCode == 200) {
            BufferedReader reader = new BufferedReader(new InputStreamReader(connection.getInputStream()));
            StringBuilder responseBody = new StringBuilder();
            String line;
            while ((line = reader.readLine()) != null) {
                responseBody.append(line);
            }
            reader.close();
            return Either.right(responseBody.toString().getBytes());
        } else {
            // Handle non-200 status codes
            String errorMsg = String.format("Request returned status: %d", statusCode);
            throw new IOException(errorMsg);
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
