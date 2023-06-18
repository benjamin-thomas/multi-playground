package org.example;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import org.example.api.Colissimo;
import org.example.domain.Delivered;
import org.example.domain.ShippingStatus;
import org.example.json.Payload;
import org.example.json.Payload.Root;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;

import static com.fasterxml.jackson.databind.DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES;

public class Main {
    public static void main(String[] args) throws IOException {
        byte[] json = getJson();
        Root root = parseJson(json);
        ShippingStatus status = toShippingStatus(root);

        System.out.println("status = " + status);
        System.out.println("Done!");
    }

    private static Root parseJson(byte[] json) throws IOException {
        ObjectMapper mapper = initMapper();
        return mapper.readValue(json, Root.class);
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
        ShippingStatus status = new ShippingStatus(root.shippingStatusCode(), delivered);
        return status;
    }

    private static byte[] getJson() throws IOException {
//        return Files.readAllBytes(Path.of("./data.json"));
        return Colissimo.fetch("API_KEY",
                "TRACKING_NUMBER");
    }

    private static ObjectMapper initMapper() {
        ObjectMapper mapper = new ObjectMapper()
                .configure(FAIL_ON_UNKNOWN_PROPERTIES, false);

        mapper.registerModule(new JavaTimeModule());
        mapper.registerModule(new Jdk8Module()); // Deserialize Optional
        return mapper;
    }

}
