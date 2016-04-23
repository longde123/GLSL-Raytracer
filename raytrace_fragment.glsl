#version 150

uniform ivec3 u_size;
uniform isamplerBuffer u_voxels;

in vec2 fragmentPosition;

out vec4 fragmentColor;

vec4 voxelAt(vec3 pos) {
  int index =
      4 * ( int(pos.x) + u_size.x *
          ( int(pos.y) + u_size.y *
          ( int(pos.z) )));
  index = clamp(0, u_size.x * u_size.y * u_size.z * 4 -1, index);
  int b = texelFetch(u_voxels, index + 0).r;
  int g = texelFetch(u_voxels, index + 1).r;
  int r = texelFetch(u_voxels, index + 2).r;
  int a = texelFetch(u_voxels, index + 3).r;
  return vec4(r / 255.0, g / 255.0, b / 255.0, a / 255.0);
}

float distToNextCellLine(float posInCell, float delta) {
  return (max(0, sign(delta)) - posInCell) / delta;
}

float stepLen(float delta) {
  return sign(delta) / delta;
}

bool isInside(vec3 pos) {
  return 0 <= pos.x && pos.x < u_size.x
    && 0 <= pos.y && pos.y < u_size.y
    && 0 <= pos.z && pos.z < u_size.z;
}

vec4 shootRay(vec3 startPos, vec3 dir, out float dist) {
  vec3 stepLine = vec3(stepLen(dir.x), stepLen(dir.y), stepLen(dir.z));
  vec3 inc = floor(sign(dir));
  vec3 cell = floor(startPos);

  vec3 posInCell = startPos - floor(startPos);
  vec3 next = vec3(
    distToNextCellLine(posInCell.x, dir.x),
    distToNextCellLine(posInCell.y, dir.y),
    distToNextCellLine(posInCell.z, dir.z));

  vec4 voxelCol = ivec4(0, 0, 0, 0);

  if (!isInside(cell)) return voxelCol;

  for (int i = 0; i < 128; i++) {
    vec3 cp = step(next, next.yzx);
    vec3 mask = cp * (vec3(1.0) - cp.zxy);

    next += stepLine * mask;
    cell += inc * mask;
    dist = dot(next, mask);

    voxelCol = voxelAt(cell);
    if (voxelCol.a != 0) return voxelCol;
  }
}

void main() {
  vec3 camera = vec3(u_size.x / 2, u_size.y / 2, 1);
  vec3 dir = normalize(vec3(1, 1, 1));

  vec2 normPos = fragmentPosition / 2 + vec2(0.5, 0.5);

  vec3 ray00 = normalize(vec3(-1, 1, 1));
  vec3 ray10 = normalize(vec3(1, 1, 1));
  vec3 ray01 = normalize(vec3(-1, -1, 1));
  vec3 ray11 = normalize(vec3(1, -1, 1));

  vec3 ray0 = mix(ray10, ray00, normPos.x);
  vec3 ray1 = mix(ray11, ray01, normPos.x);
  vec3 ray = normalize(mix(ray1, ray0, normPos.y));

  float dist;

  vec4 voxel = shootRay(camera, ray, dist);

  float normDist = clamp(dist / 20, 0, 1);

  fragmentColor = mix(voxel, vec4(0, 0, 0, 1), normDist);
}
