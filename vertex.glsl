attribute vec2 position;

varying out vec2 fragmentPosition;

void main() {
    fragmentPosition = position;
    gl_Position = vec4(position, 0.0, 1.0);
}
