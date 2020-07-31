# haskell-minithesis

This project is essentially an implementation of [minithesis](https://github.com/DRMacIver/minithesis), done to provide me a platform for continuing to learn how to write Haskell. It technically started before minithesis was created, but the release of minithesis made understanding some aspects of the core design of hypothesis easier (namely, choices).

My goal for this is learning, so there will be a lot of breaking changes and weird designs iterated through. There will also be very inconsistent development of what generators are available, etc. 

I have a bit of a thing about documentation though, so I expect it to evolve towards having pretty comprehensive documentation as I use it to develop my skills and habits around writing documentation, and because I'd like to have something in the Haskell ecosystem I can point to and say this is what documentation should look like, without having to keep pointing at code that [Lexi](https://github.com/lexi-lambda) wrote.

It almost goes without saying that I do not recommend you use this for testing your code. Just use Hedgehog instead, even though there are a few things it doesn't shrink as well.
