module Graphics.Rendering.GLSL.SDF.Utils where

import Prologue

shader_header :: String
shader_header = [s|
    precision mediump float;

    uniform float dpr;
    varying vec2 size;
    varying vec2 luv;
|]

    --uniform float aa;

sdf_utils :: String
sdf_utils = [s|
    //////////////////////////////////////
    // Combine distance field functions //
    //////////////////////////////////////

    float length      (float a, float b) { return sqrt(a * a + b * b); }
    float clamp       (float a)          { return clamp (a,0.0,1.0); }
    float invert      (float a)          { return 1.0 - a; }
    float mulInverted (float a, float b) { return invert(invert (a) * invert (b)); }

    float sdf_smoothMerge(float d1, float d2, float k) {
        float dk1 = d1 / k;
        float dk2 = d2 / k;
        float x = mulInverted(clamp(dk1), clamp(dk2));
        // float x = length(dk1, dk2);
        float h = clamp(0.5 + 0.5 * (dk2 - dk1));
        float r = (d1 * h + d2 * invert(h)) - k * h * invert(h);
        r = clamp(r, r / x, r);
        return r;
    }

    float sdf_smoothMergeDisorted(float d1, float d2, float k) {
        float h = clamp(0.5 + 0.5 * (d2 - d1) / k, 0.0, 1.0);
        return mix(d2, d1, h) - k * h * (1.0 - h);
    }

    float sdf_merge     (float d1,   float d2) { return min( d1, d2); }
    float sdf_subtract  (float d1,   float d2) { return max(-d1, d2); }
    float sdf_intersect (float d1,   float d2) { return max( d1, d2); }
    float sdf_grow      (float size, float d)  { return d - size; }
    float sdf_shrink    (float size, float d)  { return d + size; }

    //////////////////////////////
    // Distance field functions //
    //////////////////////////////

    float sdf_pie(vec2 p, float angle) {
        angle = radians(angle) / 2.0;
        vec2 n = vec2(cos(angle), sin(angle));
        return abs(p).x * n.x + p.y * n.y;
    }

    float sdf_ball(vec2 p, float radius) {
        return length(p) - radius;
    }

    float sdf_ball(vec2 p, float radius, float angle) {
        return sdf_subtract(sdf_pie(p, angle), sdf_ball(p, radius));
    }

    float sdf_sphere(vec2 p, float radius) {
        return abs(sdf_ball(p, radius));
    }

    float sdf_ellipse(vec2 p, float a, float b) {
        float a2  = a * a;
        float b2  = b * b;
        float px2 = p.x * p.x;
        float py2 = p.y * p.y;
        return (b2 * px2 + a2 * py2 - a2 * b2) / (a2 * b2);
    }

    float sdf_triangle(vec2 p, float radius) {
        return max(abs(p).x * 0.866025 + p.y * 0.5, -p.y) - radius * 0.5;
    }


    float sdf_triangle(vec2 p, float width, float height) {
        vec2 n = normalize(vec2(height, width / 2.0));
        return max(abs(p).x * n.x + p.y * n.y - (height * n.y), -p.y);
    }

    float sdf_ring(vec2 p, float radius, float width) {
        width /= 2.0;
        radius -= width;
        return abs(sdf_ball(p, radius)) - width;
    }

    float sdf_ring(vec2 p, float radius, float width, float angle) {
       return sdf_subtract(sdf_pie(p, angle), sdf_ring(p, radius, width));
    }

    float sdf_rect(vec2 p, vec2 size) {
        size /= 2.0;
        vec2 d = abs(p) - size;
        return min(max(d.x, d.y), 0.0) + length(max(d, 0.0));
    }

    float sdf_rect(vec2 p, vec2 size, float radius) {
        size /= 2.0;
        size -= vec2(radius);
        vec2 d = abs(p) - size;
        return min(max(d.x, d.y), 0.0) + length(max(d, 0.0)) - radius;
    }

    float sdf_rect(vec2 p, vec2 size, vec4 corners) {
        float tl = corners[0];
        float tr = corners[1];
        float bl = corners[2];
        float br = corners[3];

        size /= 2.0;

             if (p.x <  - size.x + tl && p.y >   size.y - tl ) { return length (p - vec2(- size.x + tl,   size.y - tl)) - tl; }
        else if (p.x >    size.x - tr && p.y >   size.y - tr ) { return length (p - vec2(  size.x - tr,   size.y - tr)) - tr; }
        else if (p.x <  - size.x + bl && p.y < - size.y + bl ) { return length (p - vec2(- size.x + bl, - size.y + bl)) - bl; }
        else if (p.x >    size.x - br && p.y < - size.y + br ) { return length (p - vec2(  size.x - br, - size.y + br)) - br; }
        else {
            vec2 d = abs(p) - size;
            return min(max(d.x, d.y), 0.0) + length(max(d, 0.0));
        }
    }

    float sdf_line(vec2 p, vec2 start, vec2 end, float width) {
        vec2 dir = start - end;
        float lngth = length(dir);
        dir /= lngth;
        vec2 proj = max(0.0, min(lngth, dot((start - p), dir))) * dir;
        return length( (start - p) - proj) - (width / 2.0);
    }

    float sdf_halfplane(vec2 p, vec2 a) {
        return a.x * p.x + a.y * p.y;
    }

    float sdf_halfplaneRight(vec2 p) {
        return sdf_halfplane(p, vec2(1.0, 0.0));
    }

    float sdf_halfplaneLeft(vec2 p) {
        return sdf_halfplane(p, vec2(-1.0, 0.0));
    }

    float sdf_halfplaneTop(vec2 p) {
        return sdf_halfplane(p, vec2(0.0, 1.0));
    }

    float sdf_halfplaneBottom(vec2 p) {
        return sdf_halfplane(p, vec2(0.0, -1.0));
    }

    ///////////////////////
    // Masks for drawing //
    ///////////////////////

    float sdf_fill(float dist) {
        return clamp(-dist, 0.0, 1.0);
    }

    // float sdf_borderOut(float width, float p) {
    //     float alpha1 = clamp(p);
    //     float alpha2 = clamp(p - width);
    //     return sdf_subtract(sdf_shrink(width,p),p);
    // }

    float sdf_borderOut(float width, float p) {
        return sdf_subtract(p + 0.5,sdf_grow(width,p));
    }

    float sdf_borderIn(float width, float p) {
        float alpha1 = clamp(p);
        float alpha2 = clamp(p - width);
        return sdf_subtract(p,sdf_grow(width,p));
    }

    float sdf_shadow(float p, float width, float exp) {
        return pow(1.0-clamp(p/width),exp);
    }

    /////////////////////
    // Transformations //
    /////////////////////

    vec2 sdf_rotateCCW(vec2 p, float a) {
        mat2 m = mat2(cos(a), sin(a), -sin(a), cos(a));
        return p * m;
    }

    vec2 sdf_rotateCW(vec2 p, float a) {
        mat2 m = mat2(cos(a), -sin(a), sin(a), cos(a));
        return p * m;
    }

    vec2 translate(vec2 p, vec2 t) {
        return p - t;
    }

    // ---------------------------

    vec3 hsv2rgb(vec3 c) {
        vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
        vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
        return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
    }

    float sdf_aa(float d) {
        float anti = fwidth(d) * aa;
        return 1.0-smoothstep(-anti, anti, d);
    }

    vec2 cartesian2polar (vec2 p) {
        return vec2(length(p), atan(p.y, p.x));
    }

    vec3 gradient_hsv (vec2 p, float step) {
        return hsv2rgb(vec3(p.x/step,1.0,1.0));
    }


    vec2 appTrans2D (vec2 p, mat4 xform) {
        return (xform * vec4(p,0.0,1.0)).xy;
    }

    float bismooth (float a, float exp) {
        if (a > 0.5) { return 1.0 - pow((1.0 - a) * 2.0, exp)/2.0; }
        else         { return pow(a * 2.0, exp)/2.0;               }
    }

    vec3 smoothMerge (float d1, float d2, vec3 c1, vec3 c2, float width) {
        return mix (c1,c2,bismooth(clamp((d1-d2+2.0*width)/(4.0*width)),2.0));
    }

    vec4 blend(vec4 dst, vec4 src) {
        dst.a += 0.000001; // avoid divide by zero
        float alpha = src.a + dst.a*(1.0-src.a);
        vec3 outcol = 1.0/alpha * (src.xyz * src.a + dst.xyz * dst.a * (1.0 - src.a));
        return vec4(outcol, alpha);
    }
|]
