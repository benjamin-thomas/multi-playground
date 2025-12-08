# Web UI Patterns

A collection of minimal, reproducible examples showcasing common web UI patterns implemented in different frameworks and languages.

## Projects

### Notifications

Demonstrates a notification system with smooth animations and independent lifecycles.

- **[React Implementation](./notifications/react/)** - Redux-based notification system
- **[PureScript Halogen Implementation](./notifications/purescript-halogen/)** - Functional implementation with Halogen
- **[Documentation](./notifications/docs/)** - Pattern explanation and comparison

## Key Features Demonstrated

- **Independent Notification Lifecycles**: Multiple notifications of the same type can coexist
- **Smooth Animations**: CSS-driven enter/exit animations without visual gaps
- **Two-step Removal**: Proper animation coordination before DOM removal
- **Random Timing**: Realistic demo with variable completion times
- **Type Safety**: Different approaches to type-safe state management

## Getting Started

Each project contains its own README with setup instructions. The implementations are minimal and focused on demonstrating the core patterns without unnecessary complexity.

## Purpose

These examples serve as:
- **Learning resources** for understanding different architectural approaches
- **Reference implementations** for copy-pasting into real projects
- **Comparison studies** between functional and imperative patterns
- **Minimal test beds** for experimenting with UI patterns