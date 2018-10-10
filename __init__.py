'''
MoreTransitions
======

Usage:

Import the transitions and use them with the ScreenManager class.

    from kivy.garden.moretransitions import PixelTransition,RippleTransition,
                                            BlurTransition,RVBTransition

    screenManager = ScreenManager(transition=PixelTransition())

or

    screenManager.transition = RVBTransition(duration=2.0)

'''

__all__ = (
    'PixelTransition',
    'RippleTransition',
    'BlurTransition',
    'RVBTransition',
    'RotateTransition'
)

from kivy.uix.screenmanager import ShaderTransition
from kivy.properties import StringProperty, OptionProperty, NumericProperty

class TileTransition(ShaderTransition):
    TILE_TRANSITION_FS = '''
        // Author: Tshirtman, with inspiration from https://gl-transitions.com/editor/GridFlip
        // ported by gre from https://gist.github.com/TimDonselaar/9bcd1c4b5934ba60087bdb55c2ea92e5

        #version 130
        $HEADER$

        uniform float t;
        uniform sampler2D tex_in;
        uniform sampler2D tex_out;

        const float s2 = sqrt(2.0);

        vec2 resolution = textureSize(tex_out, 0);

        ivec2 size = ivec2(8, 8);
        float pause= 0.1;

        vec4 bgcolor = vec4(1.0, 1.0, 1.0, 1.0);
        float randomness = 0.0;

        float rand (vec2 co) {
            return fract(sin(dot(co.xy, vec2(12.9898, 78.233))) * 43758.5453);
        }

        float getDelta(vec2 p) {
          vec2 rectanglePos = floor(vec2(size) * p);
          vec2 rectangleSize = vec2(1.0 / vec2(size).x, 1.0 / vec2(size).y);
          float top = rectangleSize.y * (rectanglePos.y + 1.0);
          float bottom = rectangleSize.y * rectanglePos.y;
          float left = rectangleSize.x * rectanglePos.x;
          float right = rectangleSize.x * (rectanglePos.x + 1.0);
          float minX = min(abs(p.x - left), abs(p.x - right));
          float minY = min(abs(p.y - top), abs(p.y - bottom));
          return min(minX, minY);
        }

        mat2 rot(float angle) {
            return mat2(
                cos(angle), -sin(angle),
                sin(angle), cos(angle)
            );
        }

        float dist_to_line(vec2 pt1, vec2 pt2, vec2 testPt) {
            vec2 lineDir = pt2 - pt1;
            vec2 perpDir = vec2(lineDir.y, -lineDir.x);
            vec2 dirToPt1 = pt1 - testPt;
            return (dot(normalize(perpDir), dirToPt1));
        }

        vec2 my_tile(vec2 p, vec2 grid) {
           // return current tile's position
           // 0___________________1
           //0|  \ /\  /\  \/ \ / |
           // |\  X  \/  \ /\  X  |
           // | \/ \ /\   X  \/ \ |
           // | /\  X  \ / \ /\  X|
           // |/  \/ \  X   /  \/ |
           // |\  /\  \/ \ / \ /\ |
           // |_\/__\_/\__X___X__\|
           //1

           vec2 A = vec2(0.0,  0.0);
           vec2 B = vec2(1.0, -1.0);

           float x = floor(dist_to_line(A, B, p * grid) / s2);

           A = vec2(0.0,  0.0);
           B = vec2(1.0, 1.0);

           // XXX why negate and add +1.0? no idea!
           float y = - floor(dist_to_line(A, B, p * grid) / s2) + 1.0;
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
    '''
    fs = StringProperty(TILE_TRANSITION_FS)


class PixelTransition(ShaderTransition):

    PIXEL_TRANSITION_FS = '''$HEADER$
        uniform float t;
        uniform sampler2D tex_in;
        uniform sampler2D tex_out;

        vec2 myround(vec2 x) {
            return vec2(floor(x.x + .5), floor(x.y + .5));
        }

        void main (void) {
            float pixels;
            float t2;
            if (t < 0.5)
                t2 = 1.0 - t * 2.0;
            else
                t2 = (t - 0.5) * 2.0;

            pixels = 5.0 + 1000.0 * t2 * t2;
            vec2 new = myround(tex_coord0.st * vec2(pixels,pixels)) /
                               vec2(pixels,pixels);

            vec4 c1 = vec4(texture2D(tex_out, new));
            vec4 c2 = vec4(texture2D(tex_in, tex_coord0.st));

            float a = min(1.0, max(0.0, (t - 0.4) / 0.2));

            gl_FragColor = c1 + vec4(a,a,a,a)*(c2-c1);
        }
    '''
    fs = StringProperty(PIXEL_TRANSITION_FS)


class RippleTransition(ShaderTransition):

    RIPPLE_TRANSITION_FS = '''$HEADER$
        uniform float t;
        uniform sampler2D tex_in;
        uniform sampler2D tex_out;

        void main (void) {
            float frequency = 20.0;
            float speed = 10.0;
            float amplitude = 0.05;
            vec2 center = vec2(0.5,0.5);
            vec2 toUV = tex_coord0.st - center;
            float distanceFromCenter = length(toUV);
            vec2 normToUV = toUV / distanceFromCenter;

            float wave = cos(frequency * distanceFromCenter - speed * t);
            float offset1 = t * wave * amplitude;
            float offset2 = (1.0 - t) * wave * amplitude;

            vec2 newUV1 = center + normToUV * vec2(distanceFromCenter+offset1,
                          distanceFromCenter + offset1);
            vec2 newUV2 = center + normToUV * vec2(distanceFromCenter+offset2,
                          distanceFromCenter + offset2);

            vec4 c1 =  vec4(texture2D(tex_out, newUV1));
            vec4 c2 =  vec4(texture2D(tex_in, newUV2));

            gl_FragColor = c1 + vec4(t,t,t,t)*(c2 - c1);
        }
    '''
    fs = StringProperty(RIPPLE_TRANSITION_FS)


class BlurTransition(ShaderTransition):

    BLUR_TRANSITION_FS = '''$HEADER$
        uniform float t;
        uniform sampler2D tex_in;
        uniform sampler2D tex_out;

        void main (void) {
            vec2 center = vec2(0.5,0.5);
            vec2 toUV = tex_coord0.st - center;

            vec4 c1 = vec4(0,0,0,0);
            int count = 24;
            float s = t * 0.02;

            for(int i=0; i<count; i++)
                c1 += texture2D(
                    tex_out,
                    tex_coord0.st - toUV * vec2(s,s) * vec2(i,i)
                );

            c1 /= vec4(count,count,count,count);
            vec4 c2 = vec4(texture2D(tex_in, tex_coord0.st));

            gl_FragColor = c1 + t*(c2 - c1);
        }
    '''
    fs = StringProperty(BLUR_TRANSITION_FS)


class RVBTransition(ShaderTransition):

    RVB_TRANSITION_FS = '''$HEADER$
        uniform float t;
        uniform sampler2D tex_in;
        uniform sampler2D tex_out;

        uniform vec2 resolution;

        void main(void)
        {
            vec2 uv = vec2(gl_FragCoord.x / resolution.x, gl_FragCoord.y /
                           resolution.y);

            float amount = 0.0;

            amount = (1.0 + sin(t*6.0)) * 0.5;
            amount *= 1.0 + sin(t*16.0) * 0.5;
            amount *= 1.0 + sin(t*19.0) * 0.5;
            amount *= 1.0 + sin(t*27.0) * 0.5;
            amount = pow(amount, 3.0);

            amount *= 0.03;

            vec3 col;
            col.r = texture2D( tex_out, vec2(uv.x+amount,uv.y) ).r * (1.0-t)
                  + texture2D( tex_in, vec2(uv.x+amount,uv.y) ).r  * t;
            col.g = texture2D( tex_out, uv ).g * (1.0-t)
                  + texture2D( tex_in, uv ).g * t;
            col.b = texture2D( tex_out, vec2(uv.x-amount,uv.y) ).b * (1.0-t)
                  + texture2D( tex_in, vec2(uv.x-amount,uv.y) ).b * t;

            col = vec3(col.r*(1.0 - amount * 0.5),
                       col.g*(1.0 - amount * 0.5),
                       col.b*(1.0 - amount * 0.5));

            gl_FragColor = vec4(col.r,col.g,col.b,1.0);
        }

    '''
    fs = StringProperty(RVB_TRANSITION_FS)

    def on_progress(self, progress):
        self.render_ctx['resolution'] = map(float, self.screen_out.size)
        super(RVBTransition, self).on_progress(progress)


class RotateTransition(ShaderTransition):
    '''Rotate transition.
    '''

    direction = OptionProperty('left', options=('left', 'right', 'up', 'down'))
    '''Direction of the transition.

    :data:`direction` is an :class:`~kivy.properties.OptionProperty`, default
    to left. Can be one of 'left', 'right', 'up' or 'down'.
    '''

    ROTATE_TRANSITION_HEADER = '''
        $HEADER$
        uniform float t;
        uniform sampler2D tex_in;
        uniform sampler2D tex_out;
        const vec4 shadow = vec4(0.0, 0.0, 0.0, 1.0);
        const float shadow_pow = 0.5;

        void main(void) {
    '''

    ROTATE_TRANSITION_FOOTER = '''
        vec4 cnew = cout;
        float light = pow(1.0-tt, shadow_pow);
        if ( tt + pos > 1.0) {
            cnew = cin;
            light=pow(tt, shadow_pow);
        }
        gl_FragColor = cnew*light*frag_color;
    }'''

    ROTATE_TRANSITION_LEFT = ROTATE_TRANSITION_HEADER + '''
        float tt = t;
        float pos = tex_coord0.x;
        vec4 cin = texture2D(tex_in,
                             vec2(1.0-(1.0-tex_coord0.x)/tt, tex_coord0.y));
        vec4 cout = texture2D(tex_out,
                              vec2(tex_coord0.x/(1.0-tt), tex_coord0.y));
    ''' + ROTATE_TRANSITION_FOOTER

    ROTATE_TRANSITION_RIGHT = ROTATE_TRANSITION_HEADER + '''
        float tt = 1.0 - t;
        float pos = tex_coord0.x;
        vec4 cin = texture2D(tex_out,
                             vec2(1.0-(1.0-tex_coord0.x)/tt, tex_coord0.y));
        vec4 cout = texture2D(tex_in,
                              vec2(tex_coord0.x/(1.0-tt), tex_coord0.y));
    ''' + ROTATE_TRANSITION_FOOTER

    ROTATE_TRANSITION_UP = ROTATE_TRANSITION_HEADER + '''
        float tt = t;
        float pos = tex_coord0.y;
        vec4 cin = texture2D(tex_in,
                             vec2(tex_coord0.x, 1.0-(1.0-tex_coord0.y)/tt));
        vec4 cout = texture2D(tex_out,
                              vec2(tex_coord0.x, tex_coord0.y/(1.0-tt)));
    ''' + ROTATE_TRANSITION_FOOTER

    ROTATE_TRANSITION_DOWN = ROTATE_TRANSITION_HEADER + '''
        float tt = 1.0 - t;
        float pos = tex_coord0.y;
        vec4 cin = texture2D(tex_out,
                             vec2(tex_coord0.x, 1.0-(1.0-tex_coord0.y)/tt));
        vec4 cout = texture2D(tex_in,
                              vec2(tex_coord0.x, tex_coord0.y/(1.0-tt)));
    ''' + ROTATE_TRANSITION_FOOTER

    fs = StringProperty(ROTATE_TRANSITION_LEFT)

    def __init__(self, **kwargs):
        self.on_direction(kwargs.get('direction', 'left'))
        super(RotateTransition, self).__init__(**kwargs)

    def on_direction(self, *largs):
        if largs[0] == 'left':
            self.fs = self.ROTATE_TRANSITION_LEFT
        if largs[0] == 'right':
            self.fs = self.ROTATE_TRANSITION_RIGHT
        if largs[0] == 'up':
            self.fs = self.ROTATE_TRANSITION_UP
        if largs[0] == 'down':
            self.fs = self.ROTATE_TRANSITION_DOWN


class FastSlideTransition(ShaderTransition):
    direction = OptionProperty('left', options=('left', 'right', 'up', 'down'))
    '''Direction of the transition.

    :data:`direction` is an :class:`~kivy.properties.OptionProperty`, default
    to left. Can be one of 'left', 'right', 'up' or 'down'.
    '''

    FAST_SLIDE_TRANSITION_UP = '''
    $HEADER$
    uniform float t;
    uniform sampler2D tex_in;
    uniform sampler2D tex_out;

    uniform vec2 resolution;

    float y2, n;
    float BLURMAX = 50.;
    float T = smoothstep(0., 1., t);
    void main(void){
        vec4 c = vec4(0., 0., 0., 0.);
        if (tex_coord0.y < 1. - T) {
            float squash = mix(.95, 1., pow(1. - t, 2.));
            float x = .5 + (tex_coord0.x - .5) / squash;
            float y = tex_coord0.y + T;

            if (0. < x && x < 1.) {
                for (n=0.; n < BLURMAX; n+=1.) {
                    y2 = y - n / resolution.y;
                    if (0. <= y2 && y2 <= 1.)
                        c += texture2D(tex_out, vec2(x, y2)) / BLURMAX;
                }
                gl_FragColor = mix(c, texture2D(tex_out, vec2(x, y)), pow(1. - t, 5.));
            } else
                gl_FragColor = vec4(0, 0, 0, 0);
        } else {
            float squash = mix(.95, 1., pow(t, 2.));
            float x = .5 + (tex_coord0.x - .5) / squash;
            float y = tex_coord0.y - 1. + T;

            if (0. < x && x < 1.) {
                for (n=0.; n < BLURMAX; n+=1.) {
                    y2 = y - n / resolution.y;
                    if (0. < y2 && y2 < 1.)
                        c += texture2D(tex_in, vec2(x, y2)) / BLURMAX;
                }
                gl_FragColor = mix(c, texture2D(tex_in, vec2(x, y)), pow(t, 5.));
            } else
                gl_FragColor = vec4(0, 0, 0, 0);
        }
    }
    '''  # noqa

    FAST_SLIDE_TRANSITION_LEFT = '''
    $HEADER$
    uniform float t;
    uniform sampler2D tex_in;
    uniform sampler2D tex_out;

    uniform vec2 resolution;

    float x2, n;
    float BLURMAX = 50.;
    float T = smoothstep(0., 1., t);
    void main(void){
        vec4 c = vec4(0., 0., 0., 0.);
        if (tex_coord0.x < 1. - T) {
            float squash = mix(.95, 1., pow(1. - t, 2.));
            float y = .5 + (tex_coord0.y - .5) / squash;
            float x = tex_coord0.x + T;

            if (0. < y && y < 1.) {
                for (n=0.; n < BLURMAX; n+=1.) {
                    x2 = x - n / resolution.x;
                    if (0. <= x2 && x2 <= 1.)
                        c += texture2D(tex_out, vec2(x2, y)) / BLURMAX;
                }
                gl_FragColor = mix(c, texture2D(tex_out, vec2(x, y)), pow(1. - t, 5.));
            } else
                gl_FragColor = vec4(0, 0, 0, 0);
        } else {
            float squash = mix(.95, 1., pow(t, 2.));
            float y = .5 + (tex_coord0.y - .5) / squash;
            float x = tex_coord0.x - 1. + T;

            if (0. < y && y < 1.) {
                for (n=0.; n < BLURMAX; n+=1.) {
                    x2 = x - n / resolution.x;
                    if (0. < x2 && x2 < 1.)
                        c += texture2D(tex_in, vec2(x2, y)) / BLURMAX;
                }
                gl_FragColor = mix(c, texture2D(tex_in, vec2(x, y)), pow(t, 5.));
            } else
                gl_FragColor = vec4(0, 0, 0, 0);
        }
    }
    '''  # noqa

    FAST_SLIDE_TRANSITION_DOWN = '''
    $HEADER$
    uniform float t;
    uniform sampler2D tex_in;
    uniform sampler2D tex_out;

    uniform vec2 resolution;

    float y2, n;
    float T = smoothstep(1., 0., t);
    float BLURMAX = 50.;
    void main(void){
        vec4 c = vec4(0., 0., 0., 0.);
        if (tex_coord0.y < 1. - T) {
            float squash = mix(.95, 1., pow(t, 2.));
            float x = .5 + (tex_coord0.x - .5) / squash;
            float y = tex_coord0.y + T;

            if (0. < x && x < 1.) {
                for (n=0.; n < BLURMAX; n+=1.) {
                    y2 = y - n / resolution.y;
                    if (0. <= y2 && y2 <= 1.)
                        c += texture2D(tex_in, vec2(x, y2)) / BLURMAX;
                }
                gl_FragColor = mix(c, texture2D(tex_in, vec2(x, y)), pow(t, 5.));
            } else
                gl_FragColor = vec4(0, 0, 0, 0);
        } else {
            float squash = mix(.95, 1., pow(1. - t, 2.));
            float x = .5 + (tex_coord0.x - .5) / squash;
            float y = tex_coord0.y - 1. + T;

            if (0. < x && x < 1.) {
                for (n=0.; n < BLURMAX; n+=1.) {
                    y2 = y - n / resolution.y;
                    if (0. <= y2 && y2 <= 1.)
                        c += texture2D(tex_out, vec2(x, y2)) / BLURMAX;
                }
                gl_FragColor = mix(c, texture2D(tex_out, vec2(x, y)), pow(1. - t, 5.));
            } else
                gl_FragColor = vec4(0, 0, 0, 0);
        }
    }
    '''  # noqa
    FAST_SLIDE_TRANSITION_RIGHT = '''
    $HEADER$
    uniform float t;
    uniform sampler2D tex_in;
    uniform sampler2D tex_out;

    uniform vec2 resolution;

    float x2, n;
    float T = smoothstep(1., 0., t);
    float BLURMAX = 50.;
    void main(void){
        vec4 c = vec4(0., 0., 0., 0.);
        if (tex_coord0.x < 1. - T) {
            float squash = mix(.95, 1., pow(t, 2.));
            float y = .5 + (tex_coord0.y - .5) / squash;
            float x = tex_coord0.x + T;

            if (0. < y && y < 1.) {
                for (n=0.; n < BLURMAX; n+=1.) {
                    x2 = x - n / resolution.x;
                    if (0. <= x2 && x2 <= 1.)
                        c += texture2D(tex_in, vec2(x2, y)) / BLURMAX;
                }
                gl_FragColor = mix(c, texture2D(tex_in, vec2(x, y)), pow(t, 5.));
            } else
                gl_FragColor = vec4(0, 0, 0, 0);
        } else {
            float squash = mix(.95, 1., pow(1. - t, 2.));
            float y = .5 + (tex_coord0.y - .5) / squash;
            float x = tex_coord0.x - 1. + T;

            if (0. < y && y < 1.) {
                for (n=0.; n < BLURMAX; n+=1.) {
                    x2 = x - n / resolution.x;
                    if (0. <= x2 && x2 <= 1.)
                        c += texture2D(tex_out, vec2(x2, y)) / BLURMAX;
                }
                gl_FragColor = mix(c, texture2D(tex_out, vec2(x, y)), pow(1. - t, 5.));
            } else
                gl_FragColor = vec4(0, 0, 0, 0);
        }
    }
    '''  # noqa
    fs = StringProperty()

    def __init__(self, **kwargs):
        self.on_direction(self, kwargs.get('direction', 'down'))
        super(FastSlideTransition, self).__init__(**kwargs)

    def on_progress(self, progress):
        self.render_ctx['resolution'] = map(float, self.screen_out.size)
        super(FastSlideTransition, self).on_progress(progress)

    def on_direction(self, *largs):
        print largs[1]
        if largs[1] == 'left':
            self.fs = self.FAST_SLIDE_TRANSITION_LEFT
        elif largs[1] == 'right':
            self.fs = self.FAST_SLIDE_TRANSITION_RIGHT
        elif largs[1] == 'up':
            self.fs = self.FAST_SLIDE_TRANSITION_UP
        elif largs[1] == 'down':
            self.fs = self.FAST_SLIDE_TRANSITION_DOWN


class ShatterTransition(ShaderTransition):
    direction = OptionProperty('left', options=('left', 'right', 'up', 'down'))
    '''Direction of the transition.

    :data:`direction` is an :class:`~kivy.properties.OptionProperty`, default
    to left. Can be one of 'left', 'right', 'up' or 'down'.
    '''
    rows = NumericProperty(10)
    cols = NumericProperty(10)

    SHATTER_TRANSITION_UP = '''
    $HEADER$
    uniform float t;
    uniform sampler2D tex_in;
    uniform sampler2D tex_out;
    uniform vec2 resolution;
    uniform float rows, cols;

    void main(void){
        float X, Y;
        X = floor(coords.x / cols);
        Y = floor(coords.y / rows);
        
    }
    '''

    def on_cols(self, *largs):
        self.render_ctx['cols'] = self.cols

    def on_rows(self, *largs):
        self.render_ctx['rows'] = self.rows

    def on_direction(self, *largs):
        print largs[1]
        if largs[1] == 'left':
            self.fs = self.SHATTER_TRANSITION_UP
        elif largs[1] == 'right':
            self.fs = self.SHATTER_TRANSITION_UP
        elif largs[1] == 'up':
            self.fs = self.SHATTER_TRANSITION_UP
        elif largs[1] == 'down':
            self.fs = self.SHATTER_TRANSITION_UP



KV = '''
FloatLayout:
    ScreenManager:
        id: sm
        Screen:
            name: '0'
            Image:
                source: '../examples/demo/pictures/images/Bubbles.jpg'
                allow_stretch: True
        Screen:
            name: '1'
            Image:
                source: '../examples/demo/pictures/images/faust_github.jpg'
                allow_stretch: True

    GridLayout:
        id: box
        size_hint: .5, .2
        pos_hint: {'center_x': .5, 'y': 0}
        cols: 2

    Button:
        text: 'previous'
        size_hint: None, None
        size: 100, 48
        pos_hint: {'center_y': .5}
        on_press:
            # if hasattr(sm.transition, 'direction'): sm.transition.direction = 'left'
            sm.current = sm.previous()

    Button:
        text: 'next'
        size_hint: None, None
        size: 100, 48
        pos_hint: {'center_y': .5, 'right': 1}
        on_press:
            # if hasattr(sm.transition, 'direction'): sm.transition.direction = 'right'
            sm.current = sm.next()
'''  # noqa

if __name__ == '__main__':
    transitions = {
        k: v(duration=.5) for k, v in globals().items()
        if isinstance(v, type) and
        issubclass(v, ShaderTransition)
    }

    from kivy.lang import Builder
    from kivy.base import runTouchApp
    from kivy.factory import Factory

    root = Builder.load_string(KV)

    def update_transition(button):
        root.ids.sm.transition = transitions[button.text]

    for k, v in transitions.items():
        btn = Factory.Button(
            text=k,
            on_press=update_transition
        )
        root.ids.box.add_widget(btn)

    runTouchApp(root)
