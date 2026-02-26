'use client'

import { useState, useEffect } from 'react'
import { motion } from 'framer-motion'
import Breadcrumbs from '@/components/ui/Breadcrumbs'
import PageHero from '@/components/ui/PageHero'
import { useInView, useCountUp } from '@/lib/hooks'

const quotes = [
  {
    text: 'Young people are not the leaders of tomorrow. They are the leaders of right now — and the systems that exclude them are weaker for it.',
    context: 'On youth political agency',
  },
  {
    text: 'Civic participation is not a privilege extended to those old enough to vote. It is a fundamental right that begins the moment a person is affected by policy — which is to say, from birth.',
    context: 'On expanding the definition of participation',
  },
  {
    text: 'We built an organization that changed legislation in two states before any of us could legally vote. That is not an anomaly. That is what happens when you stop asking young people to wait their turn.',
    context: 'On the Youth Coalition For Organ Donation',
  },
  {
    text: 'The question is never whether young people are capable of political leadership. The question is whether existing institutions are capable of making room for them.',
    context: 'On institutional barriers',
  },
]

const keyTakeaways = [
  {
    stat: 50,
    suffix: '%',
    label: 'Of global population under 30',
    description:
      'Half the world is under 30, yet youth representation in legislative bodies globally averages less than 2%. This disconnect between demographic reality and political representation is not a gap — it is a structural exclusion.',
  },
  {
    stat: 7,
    suffix: '+ Years',
    label: 'Leading the YCOD before voting age',
    description:
      'The Youth Coalition For Organ Donation was founded and led for over seven years, achieving legislative change in multiple states, all before its leadership could legally cast a ballot. This trajectory challenges the assumption that political efficacy requires formal enfranchisement.',
  },
  {
    stat: 2,
    suffix: ' States',
    label: 'Legislation influenced by youth advocacy',
    description:
      'Presumed consent organ donation legislation was advanced in two states through direct advocacy, coalition building, and testimony — work entirely conceived and executed by young people operating outside the traditional political apparatus.',
  },
  {
    stat: 18,
    suffix: '',
    label: 'Arbitrary age threshold for political voice',
    description:
      'The voting age of 18 is treated as a natural boundary for political participation, but it is an arbitrary convention. Young people are affected by tax policy, education policy, environmental policy, and healthcare policy long before they can vote on any of it.',
  },
]

function AnimatedQuote({
  quote,
  index,
}: {
  quote: (typeof quotes)[0]
  index: number
}) {
  const { ref, isInView } = useInView(0.3)
  const [revealed, setRevealed] = useState(false)

  useEffect(() => {
    if (isInView) setRevealed(true)
  }, [isInView])

  const words = quote.text.split(' ')

  return (
    <motion.div
      ref={ref}
      initial={{ opacity: 0 }}
      animate={isInView ? { opacity: 1 } : {}}
      transition={{ duration: 0.5, delay: index * 0.1 }}
      className="relative py-10 md:py-14 border-b border-white/5 last:border-b-0"
    >
      {/* TEDx red accent */}
      <div className="absolute left-0 top-10 md:top-14 w-1 h-8 bg-red-600 rounded-full" />

      <div className="pl-6 md:pl-8">
        <p className="font-serif text-xl md:text-2xl lg:text-3xl text-white/90 leading-relaxed">
          &ldquo;
          {words.map((word, i) => (
            <motion.span
              key={i}
              initial={{ opacity: 0, y: 8 }}
              animate={revealed ? { opacity: 1, y: 0 } : {}}
              transition={{
                duration: 0.3,
                delay: i * 0.03,
                ease: [0.16, 1, 0.3, 1],
              }}
              className="inline-block mr-[0.3em]"
            >
              {word}
            </motion.span>
          ))}
          &rdquo;
        </p>
        <motion.span
          initial={{ opacity: 0, y: 10 }}
          animate={revealed ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.4, delay: words.length * 0.03 + 0.2 }}
          className="inline-block mt-4 font-mono text-xs text-titanium/50 tracking-widest uppercase"
        >
          {quote.context}
        </motion.span>
      </div>
    </motion.div>
  )
}

function StatCallout({
  takeaway,
  index,
}: {
  takeaway: (typeof keyTakeaways)[0]
  index: number
}) {
  const { ref, isInView } = useInView(0.3)
  const { count, ref: countRef } = useCountUp(takeaway.stat, 2000, true)

  const displayValue = Number.isInteger(takeaway.stat)
    ? Math.round(count)
    : count.toFixed(1)

  return (
    <motion.div
      ref={ref}
      initial={{ opacity: 0, y: 40 }}
      animate={isInView ? { opacity: 1, y: 0 } : {}}
      transition={{ duration: 0.7, delay: index * 0.12, ease: [0.16, 1, 0.3, 1] }}
      className="glass rounded-xl p-6 md:p-8 hover:bg-white/10 hover:border-white/20 transition-all duration-500 group"
    >
      {/* Stat number */}
      <div className="mb-6">
        <span
          ref={countRef}
          className="font-serif text-4xl md:text-5xl text-white group-hover:text-copper transition-colors duration-500"
        >
          {displayValue}
          {takeaway.suffix}
        </span>
        <span className="block mt-2 font-mono text-xs tracking-widest uppercase text-copper">
          {takeaway.label}
        </span>
      </div>

      {/* Animated underline */}
      <motion.div
        initial={{ scaleX: 0 }}
        animate={isInView ? { scaleX: 1 } : {}}
        transition={{ duration: 0.8, delay: index * 0.12 + 0.3, ease: [0.16, 1, 0.3, 1] }}
        className="h-px bg-gradient-to-r from-copper/50 to-transparent mb-6 origin-left"
      />

      <p className="text-titanium text-sm leading-relaxed">{takeaway.description}</p>
    </motion.div>
  )
}

export default function TedPage() {
  const introRef = useInView(0.2)
  const quotesRef = useInView(0.1)
  const takeawaysRef = useInView(0.1)
  const closingRef = useInView(0.2)

  return (
    <>
      <Breadcrumbs
        items={[
          { label: 'About', href: '/about' },
          { label: 'TEDxTulane' },
        ]}
      />

      <PageHero
        title="TEDxTulane"
        subtitle="A talk on youth political participation and the structural barriers that exclude young people from the systems that govern their lives."
        label="TEDx Talk"
        variant="dark"
      />

      {/* Introduction */}
      <section className="section-padding bg-slate-950" ref={introRef.ref}>
        <div className="content-width">
          <div className="grid grid-cols-1 lg:grid-cols-5 gap-12 lg:gap-20">
            <motion.div
              className="lg:col-span-2"
              initial={{ opacity: 0, y: 30 }}
              animate={introRef.isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.7, ease: [0.16, 1, 0.3, 1] }}
            >
              <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
                The Talk
              </span>
              <h2 className="font-serif text-heading text-white">
                Youth Political Participation
              </h2>

              {/* TEDx badge */}
              <div className="mt-8 inline-flex items-center gap-3 px-4 py-2 rounded-lg bg-red-600/10 border border-red-600/20">
                <span className="font-mono text-sm font-bold tracking-wider text-red-500">
                  TEDx
                </span>
                <span className="text-sm text-titanium">Tulane University</span>
              </div>
            </motion.div>

            <motion.div
              className="lg:col-span-3"
              initial={{ opacity: 0, y: 30 }}
              animate={introRef.isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.7, delay: 0.15, ease: [0.16, 1, 0.3, 1] }}
            >
              <p className="text-lg text-titanium leading-relaxed">
                At TEDxTulane, I delivered a talk on the structural exclusion of young people from
                political systems. Drawing on direct experience founding and leading the Youth
                Coalition For Organ Donation — an organization that changed legislation in
                multiple states before any of its leadership could legally vote — the talk
                challenged the assumption that political efficacy is a function of age.
              </p>
              <p className="mt-6 text-titanium leading-relaxed">
                The central argument is straightforward: young people are not future stakeholders.
                They are current stakeholders who are systematically denied access to the
                decision-making processes that shape their education, environment, healthcare, and
                economic futures. The talk examines why this exclusion persists, what it costs
                democratic systems, and what changes when young people stop asking for permission
                and start building their own platforms for influence.
              </p>
              <p className="mt-6 text-titanium leading-relaxed">
                This is not a talk about potential. It is a talk about performance — about what
                young people have already accomplished when institutional barriers are
                circumvented rather than waited upon.
              </p>
            </motion.div>
          </div>
        </div>
      </section>

      {/* Quote Highlights */}
      <section
        className="section-padding bg-gradient-to-b from-slate-950 via-red-950/[0.03] to-slate-950 border-t border-white/5"
        ref={quotesRef.ref}
      >
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={quotesRef.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-8"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
              Highlights
            </span>
            <h2 className="font-serif text-heading text-white">From the Stage</h2>
          </motion.div>

          <div className="max-w-4xl">
            {quotes.map((quote, i) => (
              <AnimatedQuote key={i} quote={quote} index={i} />
            ))}
          </div>
        </div>
      </section>

      {/* Key Takeaways */}
      <section className="section-padding bg-slate-950 border-t border-white/5" ref={takeawaysRef.ref}>
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={takeawaysRef.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12 md:mb-16"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
              By The Numbers
            </span>
            <h2 className="font-serif text-heading text-white">Key Takeaways</h2>
            <p className="mt-4 text-lg text-titanium max-w-2xl leading-relaxed">
              The data behind the argument — quantifying the gap between youth capability and
              institutional inclusion.
            </p>
          </motion.div>

          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            {keyTakeaways.map((takeaway, i) => (
              <StatCallout key={i} takeaway={takeaway} index={i} />
            ))}
          </div>
        </div>
      </section>

      {/* Closing */}
      <section className="section-padding bg-slate-950 border-t border-white/5" ref={closingRef.ref}>
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 30 }}
            animate={closingRef.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.7, ease: [0.16, 1, 0.3, 1] }}
            className="max-w-3xl mx-auto text-center"
          >
            <div className="inline-flex items-center gap-3 px-4 py-2 rounded-lg bg-red-600/10 border border-red-600/20 mb-8">
              <span className="font-mono text-sm font-bold tracking-wider text-red-500">
                TEDx
              </span>
              <span className="text-sm text-titanium">Tulane University</span>
            </div>

            <p className="font-serif text-xl md:text-2xl text-white/90 italic leading-relaxed">
              &ldquo;The measure of a democratic system is not how well it serves those who already have
              power. It is how effectively it incorporates the voices of those it has not yet
              learned to hear.&rdquo;
            </p>

            <div className="mt-8 flex items-center justify-center gap-3">
              <div className="w-8 h-px bg-red-600/50" />
              <span className="font-mono text-xs text-titanium/50 tracking-widest uppercase">
                TEDxTulane
              </span>
              <div className="w-8 h-px bg-red-600/50" />
            </div>
          </motion.div>
        </div>
      </section>
    </>
  )
}
