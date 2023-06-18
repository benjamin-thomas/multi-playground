package org.example.json;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.time.Instant;
import java.util.List;
import java.util.Optional;

public class  Payload {
    public record Root(@JsonProperty String lang,
                       @JsonProperty("returnCode") Integer shippingStatusCode,
                       @JsonProperty Optional<Shipment> shipment) {
    }

    public record Shipment(@JsonProperty String idShip,
                           @JsonProperty("event") List<Event> events) {
    }

    public record Event(@JsonProperty String code,
                        @JsonProperty String label,
                        @JsonProperty Instant date) {
    }

}
