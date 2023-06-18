package org.example;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import io.vavr.control.Either;
import org.example.api.Colissimo;
import org.example.domain.Delivered;
import org.example.domain.ShippingStatus;
import org.example.json.Payload;
import org.example.json.Payload.Root;

import java.io.IOException;
import java.util.Optional;

import static com.fasterxml.jackson.databind.DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES;
import static io.vavr.API.*;
import static io.vavr.Patterns.$Left;
import static io.vavr.Patterns.$Right;

public class Main {
    public static void main(String[] args) {
        Object output = Match(getJson().flatMap(Main::parseJson)).of(
                Case($Left($()), leftValue -> leftValue),
                Case($Right($()), Main::toShippingStatus)
        );
        System.out.println(output);
    }

    private static void printStatus(Root root) {
        ShippingStatus status = toShippingStatus(root);
        System.out.println("status = " + status);
        System.out.println("Done!");
    }

    private static Either<String, Root> parseJson(byte[] json) {
        ObjectMapper mapper = initMapper();
        try {
            Root root = mapper.readValue(json, Root.class);
            return Either.right(root);
        } catch (IOException e) {
            return Either.left("Parse JSON error: " + e.getMessage());
        }
    }

    private static ShippingStatus toShippingStatus(Root root) {
        Optional<Delivered> delivered = root.shipment().flatMap(shipment -> {
            Optional<Payload.Event> event = shipment.events().stream().reduce((acc, evt) -> {
                if ("DI1".equals(evt.code())) {
                    return evt;
                } else {
                    return acc;
                }
            });
            return event
                    .map(evt -> new Delivered(evt.date()));
        });
        return new ShippingStatus(root.shippingStatusCode(), delivered);
    }

    private static Either<String, byte[]> getJson() {
//        return Files.readAllBytes(Path.of("./data.json"));
        return Colissimo
                .fetch("API_KEY", "TRACKING_NUMBER");
    }

    private static ObjectMapper initMapper() {
        ObjectMapper mapper = new ObjectMapper()
                .configure(FAIL_ON_UNKNOWN_PROPERTIES, false);

        mapper.registerModule(new JavaTimeModule());
        mapper.registerModule(new Jdk8Module()); // Deserialize Optional
        return mapper;
    }

}
