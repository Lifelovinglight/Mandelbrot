uniform vec2 resolution;
uniform float zoom, panx, pany;
uniform int depth;

void main(void)
{
	// vec2 res = vec2(1366.0, 768.0);
	vec2 uv = gl_FragCoord.xy / resolution.xy * zoom;
	float scale = resolution.y / resolution.x;
	uv=((uv-0.5)*5.5);
	uv.y*=scale;
	uv.y+=pany;
	uv.x-=0.5;
	uv.x+=panx;
 
 
	vec2 z = vec2(0.0, 0.0);
	vec3 c = vec3(0.0, 0.0, 0.0);
	float v;
 
	for(int i=0;i < depth;i++)
	{
 
		if(((z.x*z.x+z.y*z.y) >= 4.0)) break;
		z = vec2(z.x*z.x - z.y*z.y, 2.0*z.y*z.x) + uv;
 
 
		if((z.x*z.x+z.y*z.y) >= 2.0)
		{
			c.b = 1.0;
			c.r=float(i)/20.0;
			c.g=sin((float(i)/5.0));
		} else {
			c.r = 1.0;
			c.g = 1.0;
			c.b = 1.0;
		}
 
	}
 
 
	gl_FragColor = vec4(c,1.0);
}
