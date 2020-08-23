# haskell-minithesis

This project is essentially an implementation of [minithesis](https://github.com/DRMacIver/minithesis), done to provide me a platform for continuing to learn how to write Haskell. It technically started before minithesis was created, but the release of minithesis made understanding some aspects of the core design of hypothesis easier (namely, choices).

My goal for this is learning, so there will be a lot of breaking changes and weird designs iterated through. There will also be very inconsistent development of what generators are available, etc. 

I have a bit of a thing about documentation though, so I expect it to evolve towards having pretty comprehensive documentation as I use it to develop my skills and habits around writing documentation, and because I'd like to have something in the Haskell ecosystem I can point to and say this is what documentation should look like, without having to keep pointing at code that [Lexi](https://github.com/lexi-lambda) wrote.

It almost goes without saying that I do not recommend you use this for testing your code. Just use Hedgehog instead, even though there are a few things it doesn't shrink as well.

## How is Hypothesis/Minithesis interesting / different from other property testing libraries?
(An intro, for my friend Cyrus)

The two big property testing libraries for Haskell, QuickCheck and Hedgehog, take somewhat different approaches to generation and shrinking, and I think the way that Hypothesis does it is interesting and powerful in some ways (though a pain for implementing).

QuickCheck ties generation and shrinking to types, via typeclasses, which has the problem that if you want alternate distributions, or are generating for a particular subset of the type (such as just even integers), because you then have to create a newtype wrapper and write your own generate and shrink functions for the type, and by default people mostly don't spend the effort to write the shrink function, so you lose shrinking. Also it is possible to write the shrink function to produce values that couldn't have been generated, which fails the virtue of making invalid states unrepresentable.

Hedgehog instead has standalone generators, which compose nicely and have shrinking built in to them, and let you write a separate generator for something like even integers and have shrinking work the right way.

Both QuickCheck and Hedgehog do value based shrinking, so they shrink the values directly. This runs into a problem when a generator depends on the output of a prior generator. If we shrink the first one, it forces the regeneration of the second, which may be unlikely to still fail, so we get stuck in a bad local minimum. I have been trying to think of ways around this, but it is pretty tricky.

Hypothesis instead works by having a global sequence of choices, which are random values that are parsed into the appropriate values. Running a test either reuses existing choices, or generates new ones if appropriate, and shrinking works by making changes to the choices. This makes it possible to easily operate on the choices used by multiple generators together, which lets us implement special passes for shrinking lists that otherwise get stuck, for example. It also means that if we make a change that causes a following generator to interpret choices differently, we still have a chance of interpreting the rest in a way that produces something useful, though this is far from guaranteed (I have an idea that might improve it). By having all the state for a test case available at once, it becomes easier to avoid data dependency issues known as the "bind problem".

The hypothesis parsing approach also makes it pretty easy to get useful shrinking on new generators, since shrinking values happens by shrinking the choices, so generic shrink passes will also likely work automatically, although making shrinks really good can require dedicated effort, since you need to be careful of the order choices are made and what they imply for the shrink ordering.
