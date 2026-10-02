'use client'

import { useState, useEffect } from 'react'
import { motion } from 'framer-motion'
import Breadcrumbs from '@/components/ui/Breadcrumbs'
import PageHero from '@/components/ui/PageHero'
import CinemaEmbed from '@/components/ui/CinemaEmbed'
import { useInView, useCountUp } from '@/lib/hooks'
import { Cite, SourceList } from '@/components/ui/Sources'
import { TED_SOURCES as S } from './sources'

const quotes = [
  {
    text: 'Young people already have a stake in the decisions made about them, and the systems that leave them out are weaker for it.',
    context: 'Youth political agency',
  },
  {
    text: 'Policy affects people from birth, so the right to take part in civic life should start well before the voting age.',
    context: 'Who gets to participate',
  },
  {
    text: 'I used the Youth Coalition For Organ Donation as my main example: a group of young people who got an opt-out organ donation bill introduced in New York without waiting their turn.',
    context: 'The Youth Coalition For Organ Donation',
  },
  {
    text: 'Young people have shown they can lead politically. I asked whether existing institutions are able to make room for them.',
    context: 'Institutional barriers',
  },
]

// Sources: half the world under 30, 2.8% of MPs aged 30 or under (IPU, 2026):
// https://www.ipu.org/news/press-releases/2026-04/youth-representation-in-parliament-flatlines-first-time-in-12-years
// YCOD 3,000+ members and pending NY bill (Sept 2021):
// https://mynews13.com/fl/orlando/news/2021/09/24/wny-teens-nominated-for-american-red-cross-award-for-organ-donation-coalition
const keyTakeaways: { stat: number; suffix: string; label: string; description: string; sources?: string[] }[] = [
  {
    stat: 50,
    suffix: '%',
    label: 'Of global population under 30',
    description:
      'Half the world is under 30, yet people 30 or under hold fewer than 3% of seats in parliaments worldwide, according to the Inter-Parliamentary Union. I see that as structural exclusion.',
    sources: ['ipu-youth-2026'],
  },
  {
    stat: 7,
    suffix: '+ Years',
    label: 'Leading the YCOD',
    description:
      'I co-founded the Youth Coalition For Organ Donation at fifteen and led it for more than seven years. Its legislative work in New York was done by young people, which cuts against the idea that political influence depends on being able to vote.',
  },
  {
    stat: 3000,
    suffix: '+',
    label: 'Coalition members by 2021',
    description:
      'Our coalition grew to more than 3,000 members across the US and abroad, and our opt-out proposal became a bill in the New York State Assembly. Young people planned and carried out all of it, outside the usual political channels.',
    sources: ['red-cross-nomination'],
  },
  {
    stat: 18,
    suffix: '',
    label: 'Arbitrary age threshold for political voice',
    description:
      'The voting age of 18 is treated as the natural start of political participation, but it is a convention. Tax, education, environmental, and healthcare policy affect people long before they can vote on any of it.',
    sources: ['amendment-26'],
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
        </p>
        <motion.span
          initial={{ opacity: 0, y: 10 }}
          animate={revealed ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.4, delay: words.length * 0.03 + 0.2 }}
          className="inline-block mt-4 font-mono text-xs text-muted tracking-widest uppercase"
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
          className="font-sans font-semibold tracking-tight text-4xl md:text-5xl text-white"
        >
          {displayValue}
          {takeaway.suffix}
        </span>
        <span className="block mt-2 font-mono text-xs tracking-widest uppercase text-copper-light">
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

      <p className="text-titanium text-sm leading-relaxed">
        {takeaway.description}
        {takeaway.sources && <Cite sources={S} id={takeaway.sources} />}
      </p>
    </motion.div>
  )
}

export default function TedPage() {
  const introRef = useInView(0.2)
  const videoRef = useInView(0.1)
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
        subtitle="My talk on youth political participation and the barriers that keep young people out of the systems that govern them."
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
                The Myth of the Apolitical Youth
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
                {/* Talk title and date: https://www.ted.com/talks/evan_roden_the_myth_of_the_apolitical_youth (TEDxTulane, March 2022) */}
                In March 2022 I gave a TEDxTulane talk, The Myth of the Apolitical Youth, on how
                political systems shut young people out. I drew on my experience co-founding and
                leading the Youth Coalition For Organ Donation, whose opt-out proposal became a bill
                in New York, to challenge the idea that political influence depends on age.<Cite sources={S} id={['ted-talk', 'red-cross-nomination']} />
              </p>
              <p className="mt-6 text-titanium leading-relaxed">
                My argument was that young people are already stakeholders. Decisions about their
                education, environment, healthcare, and economic futures are made without them. In
                the talk I looked at why that exclusion persists, what it costs democracies, and
                what happens when young people stop asking permission and build their own platforms
                for influence.
              </p>
              <p className="mt-6 text-titanium leading-relaxed">
                My examples were things young people have already done by working around
                institutional barriers.
              </p>
            </motion.div>
          </div>
        </div>
      </section>

      {/* Watch the Talk */}
      <section
        className="py-section-mobile md:py-section bg-gradient-to-b from-slate-950 via-red-950/[0.02] to-slate-950 border-t border-white/5"
        ref={videoRef.ref}
      >
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 30 }}
            animate={videoRef.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.7, ease: [0.16, 1, 0.3, 1] }}
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
              Watch
            </span>
            <h2 className="font-serif text-heading text-white mb-8">The Full Talk</h2>
            <div className="max-w-4xl">
              <CinemaEmbed
                source={{ type: 'youtube', id: 'Bq3Swc8q0CY' }}
                title="TEDxTulane"
                subtitle="The Myth of the Apolitical Youth"
                aspect="16:9"
              />
            </div>
          </motion.div>
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
              Main Points
            </span>
            <h2 className="font-serif text-heading text-white">The Argument</h2>
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
              The numbers behind the argument.
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
              I argued that a democracy should be judged by how well it includes the people it has
              left out, young people among them.
            </p>

            <div className="mt-8 flex items-center justify-center gap-3">
              <div className="w-8 h-px bg-red-600/50" />
              <span className="font-mono text-xs text-muted tracking-widest uppercase">
                TEDxTulane
              </span>
              <div className="w-8 h-px bg-red-600/50" />
            </div>
          </motion.div>
        </div>
      </section>

      <SourceList sources={S} />
    </>
  )
}
