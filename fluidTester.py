import matplotlib.pyplot as plt
import matplotlib.animation as animation
from phi.flow import *
from phi.math import cos, sin, PI

# Define the vector field
v = StaggeredGrid(
    values=lambda pos: vec(
        x=cos(pos.staggered['x']) * sin(pos.staggered['y']),
        y=sin(pos.staggered['x']) * cos(pos.staggered['y']),
    ),
    extrapolation=extrapolation.PERIODIC,
    x=25,
    y=25,
    bounds=Box(x=2 * PI, y=2 * PI),
)

def make_step(v, dt=0.5):
    v = advect.semi_lagrangian(v, v, dt=dt)
    v = diffuse.implicit(v, 0.1, dt)
    v, _ = fluid.make_incompressible(v)
    return v

# Number of frames for the animation
num_frames = 20

# Create a list to store the trajectory
trj = [v]

# Generate the trajectory
for _ in range(num_frames):
    v = make_step(v)
    trj.append(v)

if __name__ == "__main__":
    plt.style.use("dark_background")

    fig, ax = plt.subplots()
    
    # Extract the first frame data for the x component of the staggered grid
    x_component = trj[0].staggered['x'].values
    img = ax.imshow(x_component.numpy('y,x'), origin='lower', cmap='viridis')

    def update(frame):
        # Extract data for the current frame
        x_component = trj[frame].staggered['x'].values
        img.set_data(x_component.numpy('y,x'))
        return [img]

    ani = animation.FuncAnimation(fig, update, frames=num_frames, blit=True)

    plt.show()