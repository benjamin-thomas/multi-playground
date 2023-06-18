package org.example.domain;

import java.util.Optional;

public record ShippingStatus(Integer shippingStatus,
                             Optional<Delivered> delivered) {
}
