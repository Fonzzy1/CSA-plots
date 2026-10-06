# claimspace

`claimspace` provides tools for representing and analysing **Claim Space Analysis (CSA)**.

Claim Space Analysis represents framing through three connected claim structures:

* **S — Salience:** which issues are made salient
* **M — Impact:** how policies are connected to issues
* **E — Evaluation:** how policies are evaluated

The package also calculates derived structures that allow observed claims to be compared with the structures implied by the rest of the claim space.

## Installation

Install the development version from a local source checkout:

```r
devtools::install_github("fonzzy1/CSA-plots")
```

Then load the package:

```r
library(claimspace)
```

## Claim-space structure

The core object is created with `claim_space()`.

The package uses the following orientation:

```text
S = issues

M = issues × policies

E = policies
```

For example:

```text
                  Fossil Fuels    Renewables
Climate Change          -12            15
Energy Prices            12             8
```

Here:

* rows of `M` are issues
* columns of `M` are policies
* `S` therefore has one value per row of `M`
* `E` has one value per column of `M`

This orientation is used consistently throughout the package.

## Creating a claim space

Here is a complete example:

```r
S_plus <- c(
    Climate_Change = 20,
    Energy_Prices = 3
)

S_minus <- c(
    Climate_Change = 0,
    Energy_Prices = 0
)

M_plus <- matrix(
    c(
        0, 15,
        12, 8
    ),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(
        c("Climate_Change", "Energy_Prices"),
        c("Fossil_Fuels", "Renewables")
    )
)

M_minus <- matrix(
    c(
        12, 0,
        0, 0
    ),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(
        c("Climate_Change", "Energy_Prices"),
        c("Fossil_Fuels", "Renewables")
    )
)

E_plus <- c(
    Fossil_Fuels = 0,
    Renewables = 23
)

E_minus <- c(
    Fossil_Fuels = 15,
    Renewables = 0
)

cs <- claim_space(
    S_plus = S_plus,
    S_minus = S_minus,
    M_plus = M_plus,
    M_minus = M_minus,
    E_plus = E_plus,
    E_minus = E_minus
)
```

The resulting net claim space is:

```r
claim_group(cs, "S", "net")

# Climate_Change Energy_Prices
#             20             3
```

```r
claim_group(cs, "M", "net")

#                Fossil_Fuels Renewables
# Climate_Change          -12         15
# Energy_Prices            12          8
```

```r
claim_group(cs, "E", "net")

# Fossil_Fuels Renewables
#          -15         23
```

## Derived structures

`claimspace` calculates four derived structures:

* `S*` — equalised salience
* `E*` — equalised evaluation
* `S^` — implied salience
* `E^` — implied evaluation

They are accessed in the same way as the observed structures:

```r
claim_group(cs, "S*")
claim_group(cs, "S^")
claim_group(cs, "E*")
claim_group(cs, "E^")
```

The derived structures are normalised to the magnitude of the corresponding observed structure, allowing their shape to be compared directly.

## Plotting

`plot_claims()` produces the complete claim-space figure using a single faceted `ggplot2` layout.

```r
plot_claims(cs)
```

By default this displays:

```text
S    S^    S*    M
                E
                E^
                E*
```

The `M` panel is always included.

You can choose which salience and evaluation structures to display:

```r
plot_claims(
    cs,
    s = c("S", "S^"),
    e = c("E", "E^")
)
```

Or display only the observed structures:

```r
plot_claims(
    cs,
    s = "S",
    e = "E"
)
```

### Plot labels

By default, the facet labels use the mathematical structure names:

```text
S    S^    S*    M
                E
                E^
                E*
```

These can be changed using the `labels` argument.

For example:

```r
plot_claims(
    cs,
    labels = c(
        S = "Salience",
        `S^` = "Implied Salience",
        `S*` = "Equalised Salience",
        M = "Impact",
        E = "Evaluation",
        `E^` = "Implied Evaluation",
        `E*` = "Equalised Evaluation"
    )
)
```

Only the labels that need to be changed have to be supplied:

```r
plot_claims(
    cs,
    labels = c(
        `S^` = "Implied",
        `S*` = "Equalised"
    )
)
```

The underlying claim-space object is not changed. `labels` only controls how the structures are displayed in the plot.

### Example plot

The example above produces a figure with shared conceptual axes: issues run down the left-hand side, while policies run across the `M` and `E` panels.

```r
plot_claims(
    cs,
    s = c("S", "S^", "S*"),
    e = c("E", "E^", "E*")
)
```

The same plotting function works for the four available measures:

```r
plot_claims(cs, measure = "positive")
plot_claims(cs, measure = "negative")
plot_claims(cs, measure = "net")
plot_claims(cs, measure = "gross")
```

## Customising plots with ggplot2

`plot_claims()` returns a regular `ggplot2` object. This means that the resulting plot can be further customised using standard `ggplot2` syntax.

For example:

```r
p <- plot_claims(cs)

p +
    ggplot2::theme_minimal()
```

or:

```r
p <- plot_claims(
    cs,
    labels = c(
        `S^` = "Implied Salience",
        `S*` = "Equalised Salience"
    )
)

p +
    ggplot2::theme(
        strip.text = ggplot2::element_text(
            size = 14,
            face = "bold"
        ),
        axis.text.x = ggplot2::element_text(
            angle = 45,
            hjust = 1
        )
    )
```

This is useful when the plot needs to be adapted for a particular publication, presentation, or visual style without changing the underlying plotting function.

### What can be changed with `theme()`

`theme()` controls the **appearance** of the plot rather than its data or structure. For example:

* `strip.text` — appearance of facet labels
* `strip.background` — backgrounds behind facet labels
* `axis.text.x` — appearance and orientation of policy labels
* `axis.text.y` — appearance of issue labels
* `axis.text` — both sets of axis labels
* `panel.spacing` — spacing between panels
* `plot.title` — plot title appearance
* `plot.subtitle` — subtitle appearance
* `plot.caption` — caption appearance
* `legend.position` — legend placement
* `plot.margin` — margins around the complete figure

For example:

```r
plot_claims(cs) +
    ggplot2::theme(
        strip.text = ggplot2::element_text(
            size = 14,
            face = "bold"
        ),
        axis.text.x = ggplot2::element_text(
            angle = 45,
            hjust = 1
        ),
        axis.text.y = ggplot2::element_text(
            size = 10
        ),
        panel.spacing = grid::unit(
            0.5,
            "lines"
        )
    )
```

This distinction is useful because `labels` and `theme()` serve different purposes:

* `labels` changes **what a structure is called**
* `theme()` changes **how that label looks**

For example:

```r
plot_claims(
    cs,
    labels = c(
        `S^` = "Implied Salience"
    )
) +
    ggplot2::theme(
        strip.text = ggplot2::element_text(
            size = 14,
            face = "italic"
        )
    )
```

Here `labels` changes `S^` to `Implied Salience`, while `theme()` controls the appearance of that facet label.

### Adding titles and captions

Because `plot_claims()` returns a `ggplot2` object, titles and captions can be added normally:

```r
plot_claims(cs) +
    ggplot2::labs(
        title = "Claim Space",
        subtitle = "Observed and derived claim structures",
        caption = "Example claim space"
    )
```

This is generally preferable to adding titles inside `plot_claims()`, because the package can provide a clean default figure while users retain control over publication-specific presentation.

### Changing the fill scale

The fill colours can be changed directly through `plot_claims()`:

```r
plot_claims(
    cs,
    low = "red",
    mid = "white",
    high = "blue"
)
```

The midpoint and limits can also be specified:

```r
plot_claims(
    cs,
    midpoint = 0,
    limits = c(-20, 20)
)
```

The `na.value` argument controls the colour used for missing values.

## Measures

Every claim structure can be examined using four measures:

| Measure    | Meaning                 |
| ---------- | ----------------------- |
| `positive` | Positive claims         |
| `negative` | Negative claims         |
| `net`      | Positive minus negative |
| `gross`    | Positive plus negative  |

For example:

```r
claim_group(
    cs,
    "M",
    measure = "net"
)
```

and:

```r
claim_group(
    cs,
    "M",
    measure = "gross"
)
```

The same measures can be used with `plot_claims()`:

```r
plot_claims(
    cs,
    measure = "gross"
)
```

## API

### `claim_space()`

Creates a claim-space object from positive and negative `S`, `M`, and `E` structures.

```r
claim_space(
    S_plus,
    S_minus,
    M_plus,
    M_minus,
    E_plus,
    E_minus
)
```

The dimensions are:

```text
S = issues

M = issues × policies

E = policies
```

Optional `issues` and `policies` arguments can be used when names need to be supplied explicitly.

### `claim_group()`

Extracts a structure from a claim-space object.

```r
claim_group(
    x,
    group,
    measure = "net"
)
```

Valid groups are:

```text
S
M
E
S*
S^
E*
E^
```

### `plot_claims()`

Plots a claim-space object.

```r
plot_claims(
    x,
    s = c("S", "S^", "S*"),
    e = c("E", "E^", "E*"),
    measure = "net",
    labels = NULL,
    show_values = TRUE,
    digits = 1,
    low = "#D08770",
    mid = "#EBCB8B",
    high = "#A3BE8C",
    midpoint = 0,
    limits = NULL,
    na.value = "grey80"
)
```

The `labels` argument is a named character vector used to change the displayed names of structures without changing the underlying claim-space object.

## Conceptual model

For an issue × policy matrix `M`:

### Equalised evaluation

The evaluation implied by the total impact associated with each policy is:

$$
E^* =
\frac{M^\top \mathbf{1}}
{\|M^\top \mathbf{1}\|}
\|E\|
$$

### Equalised salience

The salience implied by the total impact associated with each issue is:

$$
S^* =
\frac{M\mathbf{1}}
{\|M\mathbf{1}\|}
\|S\|
$$

### Implied evaluation

The evaluation implied by combining salience with impact is:

$$
\hat E =
\frac{M^\top S}
{\|M^\top S\|}
\|E\|
$$

### Implied salience

The salience implied by combining evaluation with impact is:

$$
\hat S =
\frac{ME}
{\|ME\|}
\|S\|
$$

These quantities provide different ways of examining the relationship between salience, policy impacts, and evaluation within a claim space.

## Development

Run the test suite with:

```r
testthat::test_dir("tests/testthat")
```

The package has no dependency on `patchwork`; plots are constructed using `ggplot2` and `facet_grid()`.

## License

See `LICENSE`.
