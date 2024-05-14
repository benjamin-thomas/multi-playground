#include <raylib.h>

const int WIDTH = 900;
const int HEIGHT = 900;
const int SQUARE_SIZE = 18;

#define CELLS_WIDTH 50
#define CELLS_HEIGHT 50
bool cells[CELLS_WIDTH][CELLS_HEIGHT] = {0};
bool next_cells[CELLS_WIDTH][CELLS_HEIGHT] = {0};

int get_alive_neighbors(int x, int y) {
  if (x == 0 || y == 0 || x == CELLS_WIDTH - 1 || y == CELLS_HEIGHT - 1) {
    return -1;
  }
  // clang-format off
  return cells[x - 1][y - 1] + cells[x][y - 1] + cells[x + 1][y - 1] +
         cells[x - 1][y    ]                   + cells[x + 1][y    ] +
         cells[x - 1][y + 1] + cells[x][y + 1] + cells[x + 1][y + 1];

  // clang-format on
}

bool will_live(int neighbors_alive, bool is_alive) {
  return neighbors_alive == 3 ||
         (is_alive && (neighbors_alive == 2 || neighbors_alive == 3));
}

int main(void) {
  InitWindow(HEIGHT, WIDTH, "Game of Life");
  SetTargetFPS(60);
  while (!WindowShouldClose()) {
    Vector2 mouse_pos = GetMousePosition();
    BeginDrawing();

    for (int y = 0; y < CELLS_HEIGHT; y++) {
      for (int x = 0; x < CELLS_WIDTH; x++) {
        Rectangle cell = {y * SQUARE_SIZE, x * SQUARE_SIZE, SQUARE_SIZE,
                          SQUARE_SIZE};
        bool is_hovered = CheckCollisionPointRec(mouse_pos, cell);
        bool is_left_clicked = IsMouseButtonPressed(MOUSE_LEFT_BUTTON);
        int alive_neighbors = get_alive_neighbors(x, y);
        next_cells[x][y] = will_live(alive_neighbors, cells[x][y]);

        if (is_hovered && is_left_clicked) {
          cells[x][y] = !cells[x][y];
        }

        if (cells[x][y]) {
          DrawRectangleRec(cell, YELLOW);
        } else {
          if (is_hovered) {
            DrawRectangleRec(cell, DARKGRAY);
          } else {
            DrawRectangleRec(cell, BLACK);
          }
        }

        {
          // const char *txt = TextFormat("%d", CELLS[x][y]);
          // clang-format off
          DrawText(
            TextFormat("%d", alive_neighbors),
            y * SQUARE_SIZE + 3,
            x * SQUARE_SIZE +0,
            20,
            (Color){30, 30, 30, 255}
          );
          // clang-format on
        }
      }
    }

    if (IsKeyPressed(KEY_SPACE)) {
      for (int y = 0; y < CELLS_HEIGHT; y++) {
        for (int x = 0; x < CELLS_WIDTH; x++) {
          cells[x][y] = next_cells[x][y];
        }
      }
    }

    EndDrawing();
  }
  CloseWindow();
  return 0;
}
