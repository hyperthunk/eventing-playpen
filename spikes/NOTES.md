Options
=======

- drivers (port, linked-in) 
	- possibility but already been down that route and there are pitfalls
		- OTP versus Xerces/Xalan-C threading models don't play well
		- plenty of opportunity for bugs
		- pipe based approach is too slow
		- NIF is a risk, again because of the threading models
		- build complexity is mental
	- prefer to look at pure Erlang solution now
		- not sure if it's feasible though...
	
Pure Erlang XSLT - some ideas...
---------------------------------

Top level view is something like this...

- Let SAX API _call everything in_ to a collector process as we do in lingo. 
- Whilst we're receiving data, we cache it up (in memory?) and do some indexing so we can access it later on.
- For all events received, we continuously maintain a _context path_ structure that tells us where we are (the context node) and where we've come from.
- For all our compiled XPath expressions, we maintain a __process__ that shadows the main processing context with one of its own. 
	- We'll call these the match-set for now.
	- Each member of the match set is trying to _match_ its XPath expression against the context node (see ex-1 below).
- For all events received, we apply _scatter-gather_ semantics to m


Examples
--------

1. Matching compiled xpath expression against context path.

Given an xpath expression `//ns1:foo/bar/baz[@name='judy']/child::*[@id gt 21 | string-contains(@entId, 'simple-xpath')]`, how to match it? Starting from the beginning, we notice that the first path step is a _recursive_ match any. Our first match rule is any then, and we *might* represent it using the `hamcrest_matchers:any()` matcher. Our second match rule expects a node whose local name is foo and which is scoped to the namespace 'ns1'. We will need access to a namespace context and to check for a URI that to (for our stylesheet) the same. Given a mapping for ns1 that points to google.com, if we think of the expression `ns1:foo` as in fact representing `{http://google.com}foo` then we're in good shape. We can now proceed to look at some example checks. 

Let's define our match rules for the first to path steps in a simple list of terms. 

	Rules = [any, {"http://google.com", "foo"}].

And now we can pop the next rule against each incoming event!

	%% State contains the complete rule set and some other bits
	match(_Event, [any|Remaining], State) -> 
		%% this matches because the `any` rule always does!!! 
		{Remaining, State};
	match(Event, [Rule|Remaining]=Progress, State) -> 
		case Event == Rule of
			true -> {Remaining, State};
			false -> {Progress, State}
		end;
	match(Event, [], State) ->
		%% we've matched the whole thing! 

Naturally we will need to deal with far more complex cases than this (various axes, evaluating predicates, etc) but the basics are there. Matching on axis is probably a matter of also receiving the "close" events for elements and maintaining some additional contextual state against which we can check the context node.

2. Generating a match spec for an XPath expression within a template body.

This is a special case. Because we're forgoing DOM altogether (which is probably madness but hey), we can't just compile the XPath expression in the following `<apply-templates select="child::*|sibling::*[@name='foo']" />`, because it is matching on a path relative to the context node. The _obvious_ solution (which I guess means it's either simple and obvious or plain wrong and possibly stupid) is to combine the two. Let's figure out what the match specs looks like for the following XSL fragment...

	<xsl:template match="product[@vendor='Oracle'] | product[@capped]">
		<Package commercials="true">
			<ref-id><xsl:value-of select="@vendorCode" /></ref-id>
			<xsl:apply-templates select="child::*" />
		</Package>
	</xsl:template>

The match for the anonymous template (let's call it template_$1 for now) is fairly simple:

	Match = {choice
				{{default_ns(), "product"}, {predicate, {eq, {default_ns(), '@', "vendor"}, "Oracle"}}},
				{{default_ns(), "product"}, {predicate, {exists, {default_ns(), '@', "vendor"}}}}}.

That's quite dense, but essentially we're providing a choice between two match rule definitions - either will do. The _predicate_ part is quite interesting, because not only does it represent part of the match definition, it also implicitly indicates that some actual must take place. The XPath expression `product[@capped]` matches a _product_ node, not the _capped_ attribute within it. The predicate merely enforces that only _product_ nodes defining this attribute are a valid match. The individual _match steps_ will need to be augmented with associated actions later on, possibly forming two tuples like so:

	{choice, {MatchRule1, store}=A, {MatchRule2, none}}.

Combining this with our previous and more complex match rule will be very important so that any nested XPath expressions which contain _relative paths_ from the context node can be matched successfully elsewhere! Let's look at the first one, which is an XSLT value-of call that operates on the expression `@vendorCode`. The matcher will not want to replicate the matching work of the parent context (why bother!?) so it can to _register an interest_ in the parent's match. What might this look like?

	Match = {exists, "@vendorCode"},
	Action = 'value-of', 
	Activation = {parent, store},
	{{Match, Action}, Activation}.




