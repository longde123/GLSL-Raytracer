#version 150

uniform ivec3 u_size;
uniform isamplerBuffer u_voxels;

in vec2 fragmentPosition;

out vec4 fragmentColor;

ivec4 voxelAt(ivec3 pos) {
  int index =
      4 * ( pos.x + u_size.x *
          ( pos.y + u_size.y *
          ( pos.z )));
  int b = texelFetch(u_voxels, index + 0).r;
  int g = texelFetch(u_voxels, index + 1).r;
  int r = texelFetch(u_voxels, index + 2).r;
  int a = texelFetch(u_voxels, index + 3).r;
  return ivec4(r, g, b, a);
}

float distToNextCellLine(float posInCell, float delta) {
  return (max(0, sign(delta)) - posInCell) / delta;
}

float stepLen(float delta) {
  return sign(delta) / delta;
}

bool isInside(ivec3 pos) {
  return 0 <= pos.x && pos.x < u_size.x
    && 0 <= pos.y && pos.y < u_size.y
    && 0 <= pos.z && pos.z < u_size.z;
}

ivec4 shootRay(vec3 startPos, vec3 dir, out float dist) {
  vec3 stepLine = vec3(stepLen(dir.x), stepLen(dir.y), stepLen(dir.z));
  ivec3 inc = ivec3(floor(sign(dir)));
  ivec3 cell = ivec3(floor(startPos));
  vec3 posInCell = startPos - floor(startPos);
  vec3 next = vec3(
    distToNextCellLine(posInCell.x, dir.x),
    distToNextCellLine(posInCell.y, dir.y),
    distToNextCellLine(posInCell.z, dir.z));
  ivec4 voxelCol = ivec4(0, 0, 0, 0);

  if (!isInside(cell)) return voxelCol;

  while (true) {
    vec3 cp = step(next, next.yzx);
    vec3 mask = cp * (vec3(1.0) - cp.zxy);
    
    next += stepLine * mask;
    cell += inc * ivec3(mask);
    dist = dot(next, mask);

    if (!isInside(cell)) return voxelCol;
    voxelCol = voxelAt(cell);
    if (voxelCol.a != 0) return voxelCol;
  }
}

vec3 lerp(vec3 start, vec3 end, float t) {
  return start * t + end * (1 - t);
}

void main() {
  vec3 camera = vec3(u_size.x / 2, u_size.y / 2, 1);
  vec3 dir = normalize(vec3(1, 1, 1));

  vec2 normPos = fragmentPosition / 2 + vec2(0.5, 0.5);

  vec3 ray00 = normalize(vec3(-1, 1, 1));
  vec3 ray10 = normalize(vec3(1, 1, 1));
  vec3 ray01 = normalize(vec3(-1, -1, 1));
  vec3 ray11 = normalize(vec3(1, -1, 1));

  vec3 ray0 = lerp(ray00, ray10, normPos.x);
  vec3 ray1 = lerp(ray01, ray11, normPos.x);
  vec3 ray = normalize(lerp(ray0, ray1, normPos.y));

  float dist;

  ivec4 voxel = shootRay(camera, ray, dist);

  float normDist = clamp(dist / 20, 0, 1);

  fragmentColor = mix(voxel / 255f, vec4(0, 0, 0, 1), normDist);
}
