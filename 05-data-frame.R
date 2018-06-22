Lst <- list(name = "Fred", wife="Mary", no.children = 3, child.ages = c(4, 7, 9))

# Lst[1]
# Lst[[2]]
# Lst[[4]][1]
# Lst["name"]
# Lst[["name"]]
# Lst$name
# Lst$child.ages[1]
# Lst[1]$name
# Lst["name"]$name

# Concatenation
# c(L1, L2)

state <- c("tas", "sa", "qld", "nsw", "nsw", "nt", "wa", "wa", "qld", "vic", "nsw", "vic", "qld", "qld", "sa", "tas", "sa", "nt", "wa", "vic", "qld", "nsw", "nsw", "wa", "sa", "act", "nsw", "vic", "vic", "act")
statef <- factor(state)

incomes <- c(60, 49, 40, 61, 64, 60, 59, 54, 62, 69, 70, 42, 56, 61, 61, 61, 58, 51, 48, 65, 49, 49, 41, 48, 52, 46, 59, 46, 58, 43)
incomef <- factor(incomes)

accountants <- data.frame(home = statef, loot = incomes, shot = incomef)

# as.data.frame(list)

# str(accountants)
# head(accountants)

# mapply(anyNA, accountants)

attach(accountants)
# search()

# accountants$home
# home

# ls(2)
# ls("accountants")

# detach()
# search()
