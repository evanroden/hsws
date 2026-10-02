'use client'

import { motion } from 'framer-motion'
import Breadcrumbs from '@/components/ui/Breadcrumbs'
import PageHero from '@/components/ui/PageHero'
import CinemaEmbed, { CinemaEmbedCompact } from '@/components/ui/CinemaEmbed'
import { useInView } from '@/lib/hooks'

/* ─── Equipment Data ─────────────────────────────── */

const equipment = [
  {
    category: 'Camera Systems',
    items: [
      {
        // The listed specs (Super 35, 13 stops, BRAW) are the Pocket Cinema Camera 6K (2019). The
        // full-frame "Blackmagic Cinema Camera 6K" was announced Sept 2023, after the 2020 films below.
        // https://www.cined.com/blackmagic-pocket-cinema-camera-6k-announced-super-35-sensor-and-ef-mount/
        // https://ymcinema.com/2023/09/14/blackmagic-announces-the-full-frame-cinema-camera-6k
        name: 'Blackmagic Pocket Cinema Camera 6K',
        detail: '6K Super 35 sensor, 13 stops of dynamic range, Blackmagic RAW',
      },
      {
        // https://www.bhphotovideo.com/c/product/1255307-REG/sony_alpha_a7s_ii_mirrorless.html
        name: 'Sony a7s II',
        detail: 'Full-frame mirrorless, strong low-light performance, S-Log2/S-Log3',
      },
    ],
  },
  {
    category: 'Post-Production',
    items: [
      { name: 'Adobe Premiere Pro', detail: 'Primary NLE for editorial assembly and delivery' },
      { name: 'Adobe After Effects', detail: 'Motion graphics, compositing, and visual effects' },
      { name: 'DaVinci Resolve', detail: 'Color grading, color science management, and finishing' },
      // https://www.provideocoalition.com/cinema-grade-a-new-way-to-color-grade-footage-inside-of-your-nle/
      { name: 'Cinema Grade', detail: 'Color grading plug-in that works directly on the image in the viewer' },
    ],
  },
]

/* ─── Page Component ─────────────────────────────── */

export default function CinematographyPage() {
  const heroRef = useInView(0.1)
  const reelRef = useInView(0.1)
  const narrativeRef = useInView(0.1)
  const commercialRef = useInView(0.1)
  const ambientRef = useInView(0.1)
  const audioRef = useInView(0.1)
  const funRef = useInView(0.1)
  const equipRef = useInView(0.1)

  return (
    <>
      <Breadcrumbs
        items={[
          { label: 'Studio', href: '/studio' },
          { label: 'Cinematography' },
        ]}
      />

      <PageHero
        title="Cinematography"
        subtitle="Short film, promotional, and institutional video. I learned the work as a camera operator and editor under industry professionals in New Orleans."
        label="Motion Pictures"
        variant="warm"
      />

      {/* Claiborne Avenue Productions + Tulane Freeman */}
      <section className="section-padding bg-slate-950" ref={heroRef.ref}>
        <div className="content-width">
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-12 lg:gap-20">
            <motion.div
              initial={{ opacity: 0, y: 30 }}
              animate={heroRef.isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.7, ease: [0.16, 1, 0.3, 1] }}
            >
              <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
                Production House
              </span>
              <h2 className="font-serif text-heading text-white">
                Claiborne Avenue Productions
              </h2>
              {/* Moten's credits on these films are in the locations department, not as a
                  producer/director. 12 Years a Slave: "Albert Moten, Jr., Loc asst"
                  https://catalog.afi.com/Catalog/moviedetails/69781
                  Now You See Me listed among his credits: https://www.imdb.com/name/nm4328198/
                  "Over 20 years" removed: no public source found. */}
              <p className="mt-6 text-titanium leading-relaxed">
                Albert J. Moten, Jr. is a New Orleans producer and director who runs Claiborne Avenue
                Productions. He has also worked on Hollywood productions shot in Louisiana, including{' '}
                <span className="text-white font-medium">12 Years a Slave</span> (2013), where he was
                a location assistant, and{' '}
                <span className="text-white font-medium">Now You See Me</span> (2013).
              </p>
              <p className="mt-4 text-titanium leading-relaxed">
                I worked at Claiborne Avenue as a camera operator and editor, which put me on
                professional sets for every stage of a production, from blocking and lighting on the
                day to color grading and delivery afterward. Most of how I approach a shoot, I
                learned from Moten.
              </p>
            </motion.div>

            <motion.div
              initial={{ opacity: 0, y: 30 }}
              animate={heroRef.isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.7, delay: 0.2, ease: [0.16, 1, 0.3, 1] }}
            >
              <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
                Institutional Work
              </span>
              <h2 className="font-serif text-heading text-white">
                Tulane Freeman School
              </h2>
              <p className="mt-6 text-titanium leading-relaxed">
                As a videographer for the A.B. Freeman School of Business at Tulane University, I
                produced digital marketing content: short promotional videos, faculty interviews,
                event coverage, and clips for the school&apos;s social media channels.
              </p>
              <p className="mt-4 text-titanium leading-relaxed">
                The work ran on short turnarounds and the school&apos;s brand guidelines, so each
                piece had to tell a student or faculty member&apos;s story within a fixed visual
                style.
              </p>
            </motion.div>
          </div>
        </div>
      </section>

      {/* ════════════════════════════════════════════════
          SHOWREEL — Full-width cinematic hero
          ════════════════════════════════════════════════ */}
      <section
        className="py-section-mobile md:py-section bg-gradient-to-b from-slate-950 via-slate-950/95 to-slate-950"
        ref={reelRef.ref}
      >
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 30 }}
            animate={reelRef.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.7, ease: [0.16, 1, 0.3, 1] }}
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
              Showreel
            </span>
            <h2 className="font-serif text-heading text-white mb-8">Selected Work</h2>
            <CinemaEmbed
              source={{ type: 'vimeo', id: '471739161' }}
              title="Showreel"
              subtitle="Selected cinematography & editing"
              aspect="2.35:1"
            />
          </motion.div>
        </div>
      </section>

      {/* ════════════════════════════════════════════════
          NARRATIVE FILMS
          ════════════════════════════════════════════════ */}
      <section
        className="section-padding bg-slate-950 border-t border-white/5"
        ref={narrativeRef.ref}
      >
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={narrativeRef.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
              Narrative Film
            </span>
            <h2 className="font-serif text-heading text-white">Short Films</h2>
            <p className="mt-4 text-titanium max-w-2xl leading-relaxed">
              Two short films: a poetic story and a narrated piece.
            </p>
          </motion.div>

          <div className="grid grid-cols-1 lg:grid-cols-2 gap-8">
            {/* The Bridge */}
            <motion.div
              initial={{ opacity: 0, y: 30 }}
              animate={narrativeRef.isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: 0.1 }}
            >
              <CinemaEmbed
                source={{ type: 'vimeo', id: '491626637' }}
                title="The Bridge"
                subtitle="Poetic Short Film"
                aspect="2.35:1"
              />
              <div className="mt-4 pl-1">
                <span className="font-mono text-xs text-copper tracking-widest uppercase">
                  Camera Operator / Editor
                </span>
                {/* Vimeo description: "A short story written by and starring Henry Mclaughlin,
                    shot and edited by Evan Roden." https://vimeo.com/491626637
                    "Sound design in After Effects" removed (After Effects is not an audio tool). */}
                <p className="text-titanium text-sm mt-2 leading-relaxed">
                  A poetic short story written by and starring Henry McLaughlin. I shot it handheld
                  on the Sony a7s II and edited it in Premiere Pro.
                </p>
              </div>
            </motion.div>

            {/* Plato's Cave */}
            <motion.div
              initial={{ opacity: 0, y: 30 }}
              animate={narrativeRef.isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: 0.2 }}
            >
              {/* Vimeo title "Plato's Cave (professional narration)", delivered 16:9 (426x240 oEmbed).
                  https://vimeo.com/api/oembed.json?url=https://vimeo.com/492941431 */}
              <CinemaEmbed
                source={{ type: 'vimeo', id: '492941431' }}
                title="Plato's Cave"
                subtitle="Narrated Short Film"
                aspect="16:9"
              />
              <div className="mt-4 pl-1">
                <span className="font-mono text-xs text-copper tracking-widest uppercase">
                  Director of Photography
                </span>
                <p className="text-titanium text-sm mt-2 leading-relaxed">
                  A short narrated piece built on Plato&apos;s allegory of the cave. Shot on the
                  Blackmagic Pocket Cinema Camera 6K and graded in DaVinci Resolve to a desaturated
                  palette.
                </p>
              </div>
            </motion.div>
          </div>
        </div>
      </section>

      {/* ════════════════════════════════════════════════
          COMMERCIAL & INSTITUTIONAL
          ════════════════════════════════════════════════ */}
      <section
        className="section-padding bg-gradient-to-b from-slate-950 to-slate-950 border-t border-white/5"
        ref={commercialRef.ref}
      >
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={commercialRef.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
              Commercial Work
            </span>
            <h2 className="font-serif text-heading text-white">
              Institutional &amp; Promotional
            </h2>
            {/* Was "museums, universities, and cultural institutions across Louisiana": the page
                documents one museum and one university, both in New Orleans. */}
            <p className="mt-4 text-titanium max-w-2xl leading-relaxed">
              Client video in New Orleans, including marketing work for Tulane&apos;s Freeman School
              and a promotional ad for the Louisiana Children&apos;s Museum.
            </p>
          </motion.div>

          <motion.div
            initial={{ opacity: 0, y: 30 }}
            animate={commercialRef.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6, delay: 0.1 }}
            className="max-w-3xl"
          >
            {/* This embed was labeled as the Louisiana Children's Museum ad, but YouTube 7ya0DAUe5FU is
                "Evening Cozy Background Piano Jazz to Relax or Study" (Evan Roden channel), per
                https://www.youtube.com/oembed?url=https://www.youtube.com/watch?v=7ya0DAUe5FU&format=json
                Relabeled to match the actual video until the museum spot's link is supplied.
                Museum's play-based mission: https://lcm.org/about/ */}
            <CinemaEmbed
              source={{ type: 'youtube', id: '7ya0DAUe5FU' }}
              title="Evening Cozy Background Piano Jazz"
              subtitle="Background Video"
              aspect="16:9"
            />
            <div className="mt-4 pl-1">
              <span className="font-mono text-xs text-copper tracking-widest uppercase">
                From My Channel
              </span>
              <p className="text-titanium text-sm mt-2 leading-relaxed">
                The Louisiana Children&apos;s Museum ad, made for a New Orleans museum built around
                play-based learning, isn&apos;t posted publicly, so this slot shows a background
                jazz video from my YouTube channel.
              </p>
            </div>
          </motion.div>
        </div>
      </section>

      {/* ════════════════════════════════════════════════
          AMBIENT & EXPERIMENTAL
          ════════════════════════════════════════════════ */}
      <section
        className="section-padding bg-slate-950 border-t border-white/5"
        ref={ambientRef.ref}
      >
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={ambientRef.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
              Ambient Cinema
            </span>
            <h2 className="font-serif text-heading text-white">
              4K Ambient &amp; Atmospheric
            </h2>
            {/* YouTube title: "Afternoon Upstate NY Snowy Fire Living Room with Jazz for Background
                Studying (4K, 60 FPS, HDR)". Length ("4 Hours") not publicly checkable: OWNER-CONFIRM. */}
            <p className="mt-4 text-titanium max-w-2xl leading-relaxed">
              Long-form background video, shot in 4K at 60fps and graded in HDR.
            </p>
          </motion.div>

          <motion.div
            initial={{ opacity: 0, y: 30 }}
            animate={ambientRef.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6, delay: 0.1 }}
            className="max-w-3xl"
          >
            <CinemaEmbed
              source={{ type: 'youtube', id: 'U-o0wAagNbQ' }}
              title="Afternoon in Upstate New York"
              subtitle="4K HDR &middot; 60fps &middot; 4 Hours"
              aspect="16:9"
            />
            <div className="mt-4 pl-1">
              <span className="font-mono text-xs text-copper tracking-widest uppercase">
                Cinematographer / Colorist
              </span>
              <p className="text-titanium text-sm mt-2 leading-relaxed">
                Four hours of a fireplace on a snowy afternoon, with jazz, made to play in the
                background while you study or work. I shot it on location in upstate New York and
                graded it to keep the warm firelight natural.
              </p>
            </div>
          </motion.div>
        </div>
      </section>

      {/* ════════════════════════════════════════════════
          AUDIO — WWNO Classical Radio
          ════════════════════════════════════════════════ */}
      <section
        className="section-padding bg-gradient-to-b from-slate-950 to-slate-950 border-t border-white/5"
        ref={audioRef.ref}
      >
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={audioRef.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
              Audio Production
            </span>
            <h2 className="font-serif text-heading text-white">
              WWNO Classical Radio
            </h2>
            {/* WWNO is the NPR member station for New Orleans: https://en.wikipedia.org/wiki/WWNO
                YouTube title of the embed: "WWNO Sample Show". */}
            <p className="mt-4 text-titanium max-w-2xl leading-relaxed">
              A sample program I produced for WWNO, New Orleans&apos; NPR affiliate: an hour of
              classical music with narration between pieces.
            </p>
          </motion.div>

          <motion.div
            initial={{ opacity: 0, y: 30 }}
            animate={audioRef.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6, delay: 0.1 }}
            className="max-w-3xl"
          >
            <CinemaEmbed
              source={{ type: 'youtube', id: 'rO_H8d7LbOo' }}
              title="Classical Radio Show"
              subtitle="WWNO &middot; 1 Hour"
              aspect="16:9"
            />
          </motion.div>
        </div>
      </section>

      {/* ════════════════════════════════════════════════
          JUST FOR FUN — Elevator Reviews
          ════════════════════════════════════════════════ */}
      <section
        className="section-padding bg-slate-950 border-t border-white/5"
        ref={funRef.ref}
      >
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={funRef.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <div className="flex items-center gap-4 mb-4">
              <span className="font-mono text-xs tracking-widest uppercase text-copper">
                Just For Fun
              </span>
              <div className="h-px flex-1 bg-gradient-to-r from-copper/20 to-transparent" />
            </div>
            <h2 className="font-serif text-heading text-white">
              The Elevator Review Series
            </h2>
            <p className="mt-4 text-titanium max-w-2xl leading-relaxed">
              Because sometimes you just need to review an elevator. A micro-series that brings
              cinema production values to the world&apos;s most mundane vertical transportation
              systems.
            </p>
          </motion.div>

          {/* Subtitles are the videos' actual YouTube titles (oEmbed for _6mzmQtPKyQ, TV9xw4Q0eek,
              4NqWubbW5g4). The 1:00 durations could not be checked publicly: OWNER-CONFIRM. */}
          <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
            <motion.div
              initial={{ opacity: 0, y: 30 }}
              animate={funRef.isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.5, delay: 0.1 }}
            >
              <CinemaEmbedCompact
                source={{ type: 'youtube', id: '_6mzmQtPKyQ' }}
                title="Elevator Review #1"
                subtitle="The Ideal Elevator"
                duration="1:00"
              />
            </motion.div>
            <motion.div
              initial={{ opacity: 0, y: 30 }}
              animate={funRef.isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.5, delay: 0.2 }}
            >
              <CinemaEmbedCompact
                source={{ type: 'youtube', id: 'TV9xw4Q0eek' }}
                title="Elevator Review #2"
                subtitle="A Step Down"
                duration="1:00"
              />
            </motion.div>
            <motion.div
              initial={{ opacity: 0, y: 30 }}
              animate={funRef.isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.5, delay: 0.3 }}
            >
              <CinemaEmbedCompact
                source={{ type: 'youtube', id: '4NqWubbW5g4' }}
                title="Elevator Review #3"
                subtitle="Return to Normalcy"
                duration="1:00"
              />
            </motion.div>
          </div>
        </div>
      </section>

      {/* ════════════════════════════════════════════════
          EQUIPMENT & POST-PRODUCTION
          ════════════════════════════════════════════════ */}
      <section className="section-padding bg-slate-950 border-t border-white/5" ref={equipRef.ref}>
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={equipRef.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
              Technical Specifications
            </span>
            <h2 className="font-serif text-heading text-white mb-12">
              Equipment &amp; Post-Production
            </h2>
          </motion.div>

          <div className="grid grid-cols-1 md:grid-cols-2 gap-8">
            {equipment.map((group, gi) => (
              <motion.div
                key={group.category}
                initial={{ opacity: 0, y: 30 }}
                animate={equipRef.isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.6, delay: gi * 0.15 }}
                className="glass rounded-xl p-6 md:p-8"
              >
                <h3 className="font-mono text-sm tracking-widest uppercase text-copper mb-6">
                  {group.category}
                </h3>
                <div className="space-y-6">
                  {group.items.map((item) => (
                    <div
                      key={item.name}
                      className="border-l-2 border-white/10 pl-4 hover:border-copper/50 transition-colors duration-300"
                    >
                      <h4 className="text-white font-medium">{item.name}</h4>
                      <p className="text-titanium text-sm mt-1">{item.detail}</p>
                    </div>
                  ))}
                </div>
              </motion.div>
            ))}
          </div>
        </div>
      </section>
    </>
  )
}
