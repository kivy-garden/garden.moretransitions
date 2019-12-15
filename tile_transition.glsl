// License: MIT
// Author: TimDonselaar
// ported by gre from https://gist.github.com/TimDonselaar/9bcd1c4b5934ba60087bdb55c2ea92e5
// modified by tshirtman from https://gl-transitions.com/editor/GridFlip

#version 130
$HEADER$

uniform float t;
uniform sampler2D tex_in;
uniform sampler2D tex_out;

// screen aspect ratio
ivec2 size = ivec2(16, 16);
float pause= 0.1;


const float s2 = sqrt(2.);

vec2 resolution = textureSize(tex_out, 0);
float AR = resolution.x / resolution.y;

vec4 bgcolor = vec4(1.0, 1.0, 1.0, 1.0);
float randomness = 0.0;

float rand (vec2 co) {
    return fract(sin(dot(co.xy, vec2(12.9898, 78.233))) * 43758.5453);
}

float dist_to_line(vec2 pt1, vec2 pt2, vec2 testPt) {
    vec2 lineDir = pt2 - pt1;
    vec2 perpDir = vec2(lineDir.y, -lineDir.x);
    vec2 dirToPt1 = pt1 - testPt;
    return (dot(normalize(perpDir), dirToPt1));
}

vec2 my_tile(vec2 p, vec2 grid) {
   // return current tile's position
   //
   // NOT TO SCALE         __
   //                     / |
   //                    /  |
   //                   /   |
   //                  /    |
   //                 /     |
   //                /      |
   //               /       |
   //              /        |
   //             /         |
   //  X reference line     |
   //           /           |
   //          /            |
   //         /             |AR
   //        /              |
   //       /               |
   //      /                |
   //     /                 |
   //    /                  |
   //   /                   |
   // 0/__________________1_v_
   //0|\11/\22/\33/\44/\55| ^
   // |1\/21\/32\/43\/54\/| |
   // -1/\  /\  /\  /\  /\| |
   // |/2 \/31\/42\/53\/64| |
   // |\-1/\  /\  /\  /\  | |
   // | \/3 \/41\/52\/63\/| |
   // |_/\-1/\ _/\__/\__/\| |
   //1        \             |
   //          \            |
   //           \           |AR
   //            \          |
   //   y reference line    |
   //              \        |
   //               \       |
   //                \      |
   //                 \     |
   //                  \    |
   //                   \   |
   //                    \__v

   vec2 A = vec2(0.0, 0.0);
   vec2 B = vec2(1.0, -1.0);

   float x = floor(dist_to_line(A, B, p / vec2(1.0, AR) * grid) / s2);

   A = vec2(0.0, 0.0);
   B = vec2(1.0, 1.0);

   // vflip to switch coords system
   float y = - floor(dist_to_line(A, B, p / vec2(1.0, AR) * grid) / s2) + 1.0;
   return vec2(x, y);
}

void main() {
    vec2 p = gl_FragCoord.xy / resolution.xy;
    float currentProg = (t - pause) / (1.0 - pause * 2.0);
    vec2 q = p;
    vec2 dp = 1. / resolution;

    vec2 t1 = my_tile(p, vec2(float(size.x), float(size.y)));
    float r = rand(vec2(t1)) - randomness;
    float cp = smoothstep(0.0, 1.0 - r, currentProg);

    // XXX pretty sure the issue is here now, computing this value
    // correctly should fix the issue, the current value is pretty weird

    vec2 rectanglePos = vec2(
        t1.x / float(size.x) + t1.y / float(size.y),
        t1.x / float(size.y) + t1.y / float(size.x)
    );

    float rectangleSize = 2. / (float(size.x));

    float offset = rectanglePos.x - .5 * rectangleSize;

    p.x = (p.x - offset) / abs(cp - 0.5) * 0.5 + offset;

    vec2 t2 = my_tile(p, vec2(float(size.x), float(size.y)));
    if (t1.x != t2.x || t1.y != t2.y)
        gl_FragColor = bgcolor;
    else {
        vec4 a = texture2D(tex_out, p);
        vec4 b = texture2D(tex_in, p);

        float s = 1.;
        float sr = 1.;
        if (t < pause) {
            sr = t / pause;
        }
        else if (t > 1.0 - pause) {
            sr = 1. - (t - (1. - pause)) / pause;
        }
        for (int x = -1; x <= 1; x++) {
            for (int y = -1; y <= 1; y++) {
                vec2 np = p + vec2(float(x), float(y)) * dp;
                vec2 t3 = my_tile(np, vec2(float(size.x), float(size.y)));
                if (t3.x != t2.x || t3.y != t2.y)
                    s -= sr * 1. / 9.;
            }
        }
        gl_FragColor = mix(bgcolor, mix(b, a, step(cp, 0.5)), s);
    }
}
